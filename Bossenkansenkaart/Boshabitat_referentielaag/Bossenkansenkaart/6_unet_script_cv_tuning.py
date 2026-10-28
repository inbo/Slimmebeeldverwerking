import os
import glob
import json
import random
from pathlib import Path
import numpy as np
import torch
import torch.nn as nn
import torch.optim as optim
from torch.utils.data import Dataset, DataLoader
from torchmetrics import JaccardIndex
import albumentations as A
import segmentation_models_pytorch as smp
from tqdm import tqdm


# ==========================================
# 1. Albumentations Transforms
# ==========================================
def get_train_transforms():
    """Returns spatial transforms for training dataset."""
    return A.Compose(
        [
            A.HorizontalFlip(p=0.5),
            A.VerticalFlip(p=0.5),
        ],
        additional_targets={'valid_mask': 'mask'}
    )


# ==========================================
# 2. PyTorch Dataset Class
# ==========================================
class FastPreprocessedDataset(Dataset):
    def __init__(self, pt_file_paths, transform=None):
        self.pt_file_paths = pt_file_paths
        self.transform = transform

    def __len__(self):
        return len(self.pt_file_paths)

    def __getitem__(self, idx):
        data = torch.load(self.pt_file_paths[idx])
        x_tensor, target_tensor, valid_mask_tensor = data['x'], data['target'], data['valid_mask']

        if self.transform is not None:
            x_data = x_tensor.permute(1, 2, 0).numpy()
            target = target_tensor.numpy()
            valid_mask = valid_mask_tensor.numpy()

            augmented = self.transform(
                image=x_data,
                mask=target,
                valid_mask=valid_mask
            )

            x_tensor = torch.from_numpy(augmented['image']).permute(2, 0, 1)
            target_tensor = torch.from_numpy(augmented['mask']).long()
            valid_mask_tensor = torch.from_numpy(augmented['valid_mask'])

        return x_tensor, target_tensor, valid_mask_tensor


# ==========================================
# 3. UNet Model & Loss Function
# ==========================================
def build_unet_model(in_channels=35, num_classes=10):
    """
    Builds UNet model with 35 input channels using segmentation_models_pytorch.
    """
    model = smp.Unet(
        encoder_name="resnet34",
        encoder_weights=None,  # Trained from scratch due to 35 input channels
        in_channels=in_channels,
        classes=num_classes
    )
    return model


class MaskedCrossEntropyLoss(nn.Module):
    """
    Cross-Entropy loss handling ignored labels (-100).
    """
    def __init__(self, ignore_index=-100):
        super().__init__()
        self.criterion = nn.CrossEntropyLoss(ignore_index=ignore_index, reduction='mean')

    def forward(self, logits, target):
        return self.criterion(logits, target)


# ==========================================
# 4. Helper to resolve .pt paths from Lookup
# ==========================================
def get_pt_file_paths(selected_blocks, lookup_dict, preprocessed_dirs):
    """Gathers full file paths for all .pt tiles belonging to selected_blocks."""
    pt_path_map = {}
    for prep_dir in preprocessed_dirs:
        for pt_path in Path(prep_dir).glob("*.pt"):
            pt_path_map[pt_path.name] = str(pt_path)

    resolved_paths = []
    for block in selected_blocks:
        pt_filenames = lookup_dict.get(block, [])
        for fname in pt_filenames:
            if fname in pt_path_map:
                resolved_paths.append(pt_path_map[fname])
            else:
                print(f"Warning: {fname} from block {block} not found in preprocessed directories.")

    return sorted(resolved_paths)


# ==========================================
# 5. Training & Evaluation Engine with Metrics
# ==========================================
def train_one_epoch(model, dataloader, criterion, optimizer, device):
    model.train()
    running_loss = 0.0
    for x, target, _ in dataloader:
        x, target = x.to(device), target.to(device)
        optimizer.zero_grad()
        outputs = model(x)
        loss = criterion(outputs, target)
        loss.backward()
        optimizer.step()
        running_loss += loss.item() * x.size(0)
    return running_loss / len(dataloader.dataset)


def evaluate_with_metrics(model, dataloader, criterion, num_classes, device, ignore_index=-100):
    """
    Evaluates loss, mIoU (Jaccard Index), and Average Dice Score over the dataloader.
    Formula conversion: Dice = 2 * IoU / (1 + IoU)
    """
    model.eval()
    running_loss = 0.0
    
    jaccard_metric = JaccardIndex(
        task="multiclass", 
        num_classes=num_classes, 
        ignore_index=ignore_index
    ).to(device)

    with torch.no_grad():
        for x, target, _ in dataloader:
            x, target = x.to(device), target.to(device)
            outputs = model(x)
            loss = criterion(outputs, target)
            running_loss += loss.item() * x.size(0)

            # Predictions for metrics
            preds = torch.argmax(outputs, dim=1)
            jaccard_metric.update(preds, target)

    total_loss = running_loss / len(dataloader.dataset)
    miou = jaccard_metric.compute().item()
    
    # Calculate average Dice score directly from mIoU
    dice_score = (2.0 * miou) / (1.0 + miou) if (1.0 + miou) > 0 else 0.0

    return total_loss, miou, dice_score


# ==========================================
# 6. Early Stopping Implementation
# ==========================================
class EarlyStopping:
    def __init__(self, patience=10, delta=1e-4, save_path="best_model.pt"):
        self.patience = patience
        self.delta = delta
        self.save_path = save_path
        self.counter = 0
        self.best_loss = None
        self.early_stop = False

    def __call__(self, val_loss, model):
        if self.best_loss is None:
            self.best_loss = val_loss
            self.save_checkpoint(model)
        elif val_loss < self.best_loss - self.delta:
            self.best_loss = val_loss
            self.save_checkpoint(model)
            self.counter = 0
        else:
            self.counter += 1
            if self.counter >= self.patience:
                self.early_stop = True

    def save_checkpoint(self, model):
        torch.save(model.state_dict(), self.save_path)


# ==========================================
# 7. Main LOBO-CV & Hyperparameter Search
# ==========================================
def main():
    SEED = 42
    random.seed(SEED)
    np.random.seed(SEED)
    torch.manual_seed(SEED)

    device = torch.device("cuda" if torch.cuda.is_available() else "cpu")
    print(f"Using device: {device}")

    # Directories and Lookup Setup
    preprocessed_dirs = [
        "E:/preprocessed_train",
        "E:/preprocessed_val",
        "E:/preprocessed_test",
    ]
    lookup_file = "block_tile_lookup.json"

    if not os.path.exists(lookup_file):
        raise FileNotFoundError(f"Lookup file '{lookup_file}' not found.")

    with open(lookup_file, "r") as f:
        lookup_table = json.load(f)

    all_blocks = sorted(list(lookup_table.keys()))
    print(f"Available blocks ({len(all_blocks)}): {all_blocks}")

    # Hyperparameter Grid Setup
    batch_sizes = [4, 8]
    learning_rates = [1e-3, 1e-4]
    max_epochs = 100
    patience = 10
    num_classes = 10  # Match target class count

    output_dir = "./lobo_cv_results"
    os.makedirs(output_dir, exist_ok=True)

    overall_results = []

    # Leave-One-Block-Out CV Loop
    for test_idx, test_block in enumerate(all_blocks):
        print(f"\n==================================================")
        print(f"LOBO Fold {test_idx + 1}/{len(all_blocks)} | Test Block: {test_block}")
        print(f"==================================================")

        remaining_blocks = [b for b in all_blocks if b != test_block]
        
        # Randomly select a validation block from remaining blocks
        val_block = random.choice(remaining_blocks)
        train_blocks = [b for b in remaining_blocks if b != val_block]

        print(f"Train Blocks: {train_blocks}")
        print(f"Val Block:    [{val_block}]")
        print(f"Test Block:   [{test_block}]")

        train_paths = get_pt_file_paths(train_blocks, lookup_table, preprocessed_dirs)
        val_paths = get_pt_file_paths([val_block], lookup_table, preprocessed_dirs)
        test_paths = get_pt_file_paths([test_block], lookup_table, preprocessed_dirs)

        train_dataset = FastPreprocessedDataset(train_paths, transform=get_train_transforms())
        val_dataset = FastPreprocessedDataset(val_paths, transform=None)
        test_dataset = FastPreprocessedDataset(test_paths, transform=None)

        best_val_loss_for_fold = float("inf")
        best_val_miou = 0.0
        best_val_dice = 0.0
        best_hp_combo = None

        # Hyperparameter Search
        for bs in batch_sizes:
            for lr in learning_rates:
                print(f"\n--- Tuning HP: Batch Size={bs}, Learning Rate={lr} ---")

                train_loader = DataLoader(train_dataset, batch_size=bs, shuffle=True, num_workers=0, pin_memory=True)
                val_loader = DataLoader(val_dataset, batch_size=bs, shuffle=False, num_workers=0, pin_memory=True)

                model = build_unet_model(in_channels=35, num_classes=num_classes).to(device)
                criterion = MaskedCrossEntropyLoss(ignore_index=-100)
                optimizer = optim.Adam(model.parameters(), lr=lr)

                checkpoint_path = os.path.join(output_dir, f"model_fold_{test_block}_bs{bs}_lr{lr}.pt")
                early_stopping = EarlyStopping(patience=patience, save_path=checkpoint_path)

                for epoch in range(1, max_epochs + 1):
                    train_loss = train_one_epoch(model, train_loader, criterion, optimizer, device)
                    val_loss, val_miou, val_dice = evaluate_with_metrics(
                        model, val_loader, criterion, num_classes, device
                    )

                    print(
                        f"Epoch {epoch:03d}/{max_epochs:03d} | "
                        f"Train Loss: {train_loss:.4f} | "
                        f"Val Loss: {val_loss:.4f} | "
                        f"Val mIoU: {val_miou:.4f} | "
                        f"Val Dice: {val_dice:.4f}"
                    )

                    early_stopping(val_loss, model)
                    if early_stopping.early_stop:
                        print(f"Early stopping triggered at epoch {epoch}")
                        break

                if early_stopping.best_loss < best_val_loss_for_fold:
                    best_val_loss_for_fold = early_stopping.best_loss
                    best_val_miou = val_miou
                    best_val_dice = val_dice
                    best_hp_combo = {
                        'batch_size': bs,
                        'learning_rate': lr,
                        'checkpoint': checkpoint_path
                    }

        # Evaluate final test set for this fold
        print(f"\nEvaluating fold test performance with best HPs: {best_hp_combo}")
        best_model = build_unet_model(in_channels=35, num_classes=num_classes).to(device)
        best_model.load_state_dict(torch.load(best_hp_combo['checkpoint']))

        test_loader = DataLoader(test_dataset, batch_size=best_hp_combo['batch_size'], shuffle=False, num_workers=0)
        criterion = MaskedCrossEntropyLoss(ignore_index=-100)
        
        test_loss, test_miou, test_dice = evaluate_with_metrics(
            best_model, test_loader, criterion, num_classes, device
        )

        print(
            f"Test Results for fold ({test_block}) -> "
            f"Loss: {test_loss:.4f} | mIoU: {test_miou:.4f} | Dice: {test_dice:.4f}"
        )

        # Store complete fold result
        fold_result = {
            'test_block': test_block,
            'val_block': val_block,
            'train_blocks': train_blocks,
            'best_hyperparameters': best_hp_combo,
            'validation': {
                'loss': best_val_loss_for_fold,
                'mIoU': best_val_miou,
                'avg_dice_score': best_val_dice
            },
            'test': {
                'loss': test_loss,
                'mIoU': test_miou,
                'avg_dice_score': test_dice
            }
        }
        overall_results.append(fold_result)

    # Export overall cross-validation metrics
    summary_path = os.path.join(output_dir, "cv_results_summary.json")
    with open(summary_path, "w") as f:
        json.dump(overall_results, f, indent=4)

    print("\n==================================================")
    print("LOBO Cross-Validation Complete!")
    print(f"Summary saved to: {summary_path}")
    print("==================================================")


if __name__ == "__main__":
    main()
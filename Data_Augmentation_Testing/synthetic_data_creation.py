from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split

from sdv.metadata import SingleTableMetadata
from sdv.single_table import TVAESynthesizer, CTGANSynthesizer

# -----------------------------
# Load dataset
# -----------------------------
iris = load_iris(as_frame=True)

df = iris.frame
df.columns = [
    "sepal_length",
    "sepal_width",
    "petal_length",
    "petal_width",
    "species"
]

print(df.head())

# -----------------------------
# Train/Test split
# -----------------------------
train_df, test_df = train_test_split(
    df,
    test_size=0.2,
    random_state=42,
    stratify=df["species"]
)

# -----------------------------
# Create metadata
# -----------------------------
metadata = SingleTableMetadata()
metadata.detect_from_dataframe(train_df)

# ======================================================
# TVAE
# ======================================================

print("\nTraining TVAE...")

tvae = TVAESynthesizer(metadata)
tvae.fit(train_df)

synthetic_tvae = tvae.sample(200)

print("\nTVAE Samples")
print(synthetic_tvae.head())

# ======================================================
# CTGAN
# ======================================================

print("\nTraining CTGAN...")

ctgan = CTGANSynthesizer(metadata)
ctgan.fit(train_df)

synthetic_ctgan = ctgan.sample(200)

print("\nCTGAN Samples")
print(synthetic_ctgan.head())

# ======================================================
# Save
# ======================================================

synthetic_tvae.to_csv("synthetic_tvae.csv", index=False)
synthetic_ctgan.to_csv("synthetic_ctgan.csv", index=False)

print("\nSaved CSV files.")
import pandas as pd
import matplotlib.pyplot as plt
import re
from collections import Counter

# -----------------------------
# Load and deduplicate dataset
# -----------------------------
df = pd.read_excel("Copy of Systematic mapping data extraction form (Responses).xlsx")
#df = df.drop_duplicates(subset=["Study ID (format: first author_year_letterIfNeeded )"])

# -----------------------------
# Panel A: Top AI Algorithms
# -----------------------------
algo_col = "Which specific AI algorithms are mentioned?"
algos = []

for v in df[algo_col].dropna():
    for p in re.split(r",|;|/|\n", str(v).lower()):
        p = p.strip()
        if p:
            algos.append(p)

algo_col = "If other algorithm, specify:"        
for v in df[algo_col].dropna():
    for p in re.split(r",|;|/|\n", str(v).lower()):
        p = p.strip()
        if p:
            algos.append(p)

def normalize_algo(a: str):
    if "cnn" in a or "convolution" in a:
        return "CNNs"
    if "svm" in a or "support vector" in a:
        return "SVMs"
    if "random forest" in a:
        return "Random forests"
    if "rnn" in a or "recurrent" in a:
        return "RNNs"
    if "decision tree" in a:
        return "Decision trees"
    if "boost" in a:
        return "Boosted trees"
    if "transformer" in a or "llm" in a:
        return "Transformers / LLMs"
    if "knn" in a or "KNN" in a or "kNN" in a:
        return "kNN"
    if "Autoencoder" in a or "autoencoder" in a:
        return "autoencoder"
    if "kmeans" in a:
        return "kmeans"
    if "hidden markov model" in a or "Hidden markov model" in a or "Hidden Markov models" in a:
        return "Hidden Markov models"
    return None

normed_algos = [normalize_algo(a) for a in algos]
normed_algos = [a for a in normed_algos if a is not None]

algo_counts = Counter(normed_algos).most_common(10)
algo_names = [x[0] for x in algo_counts]
algo_vals = [x[1] for x in algo_counts]
print(algo_vals)

# -----------------------------
# Panel B: Coverage by Source of Data
# -----------------------------
source_col = "Source of data used in primary studies reviewed"
source_counts = df[source_col].value_counts()

source_label_map = {
    "Field-collected": "Field",
    "Public repository": "Repository",
    "Field-collected, Public repository": "Field + Repository",
    "Lab-generated, Public repository": "Lab + Repository",
    "Field-collected, Lab-generated": "Field + Lab",
    "Field-collected, Lab-generated, Public repository": "Field + Lab + Repository",
    "Not specified": "Not specified",
    "Unclear": "Unclear",
}
source_counts.index = [source_label_map.get(x, x) for x in source_counts.index]
print(source_counts)

# -----------------------------
# Panel C: Number of Entries by Data Modality
# -----------------------------
modality_col = "Categories of the data used for the AI models"
mod_counts = df[modality_col].value_counts()

mod_label_map = {
    "Images": "Images",
    "Images, Numeric": "Images + Numeric",
    "Numeric": "Numeric",
    "Audio, Images, Numeric": "Audio + Images + Numeric",
    "Video, Images, Numeric": "Video + Images + Numeric",
    "Video, Images": "Video + Images",
    "Audio": "Audio",
    "Audio, Video, Images, Numeric": "Audio + Video + Images + Numeric",
    "Audio, Video, Images": "Audio + Video + Images",
}
mod_counts.index = [mod_label_map.get(x, x) for x in mod_counts.index]
print(mod_counts)

# -----------------------------
# Figure styling improvements
# -----------------------------
plt.rcParams.update({
    "font.family": "DejaVu Sans",
    "font.size": 10,
    "axes.titlesize": 11,
    "axes.labelsize": 10,
    "xtick.labelsize": 9,
    "ytick.labelsize": 9,
    "pdf.fonttype": 42,   # Better text handling in PDFs
    "ps.fonttype": 42,
})

fig, axes = plt.subplots(
    3, 1,
    figsize=(10, 8),
    gridspec_kw={"wspace": 0.45, "hspace": 0.55}
)

# Common axis cleanup
for ax in axes.flat:
    ax.spines["top"].set_visible(False)
    ax.spines["right"].set_visible(False)

# -----------------------------
# Panel A
# -----------------------------
#ax = axes[0, 0]
ax = axes[0]
ax.barh(algo_names, algo_vals)
ax.invert_yaxis()
ax.set_xlabel("Number of Reviews")
ax.set_title("Top AI Algorithms Mentioned", pad=8)
ax.text(
    -0.14, 1.08, "A",
    transform=ax.transAxes,
    fontsize=14,
    fontweight="bold",
    va="top",
    ha="left"
)

# -----------------------------
# Panel C
# -----------------------------
#ax = axes[2, 0]
ax = axes[2]
ax.barh(source_counts.index.astype(str), source_counts.values)
ax.invert_yaxis()
ax.set_xlabel("Number of Reviews")
ax.set_title("Coverage by Source of Data", pad=8)
ax.text(
    -0.14, 1.08, "C",
    transform=ax.transAxes,
    fontsize=14,
    fontweight="bold",
    va="top",
    ha="left"
)

# -----------------------------
# Panel B
# -----------------------------
#ax = axes[1, 0]
ax = axes[1]
ax.barh(mod_counts.index.astype(str), mod_counts.values)
ax.invert_yaxis()
ax.set_xlabel("Number of Reviews")
ax.set_title("Number of Entries by Data Modality", pad=8)
ax.text(
    -0.14, 1.08, "B",
    transform=ax.transAxes,
    fontsize=14,
    fontweight="bold",
    va="top",
    ha="left"
)

# -----------------------------
# Empty panel D for balance
# -----------------------------
#axes[1, 1].axis("off")

# -----------------------------
# Save outputs
# -----------------------------
plt.savefig("AI_Ecology_Review_Figure_Panels_Improved.pdf", bbox_inches="tight")
plt.savefig("AI_Ecology_Review_Figure_Panels_Improved.png", dpi=600, bbox_inches="tight")

plt.show()
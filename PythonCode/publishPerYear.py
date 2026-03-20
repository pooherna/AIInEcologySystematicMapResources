import pandas as pd
import matplotlib.pyplot as plt

# Load the dataset
df = pd.read_excel("Copy of Systematic mapping data extraction form (Responses).xlsx")

# Remove duplicate reviews using Study ID
#df = df.drop_duplicates(subset=["Study ID (format: first author_year_letterIfNeeded )"])

# Extract year from Study ID
years = df["Study ID (format: first author_year_letterIfNeeded )"].str.extract(r'(\d{4})')
years = pd.to_numeric(years[0], errors="coerce")

# Count reviews per year
year_counts = years.value_counts().sort_index()

print(year_counts)

# Plot
plt.figure(figsize=(7,5))
plt.bar(year_counts.index, year_counts.values)

plt.xlabel("Publication Year")
plt.ylabel("Number of Reviews")
plt.title("Included Reviews by Publication Year")

plt.xticks(year_counts.index)
plt.tight_layout()

# Save figure
plt.savefig("included_reviews_by_year.png", dpi=300)

plt.show()
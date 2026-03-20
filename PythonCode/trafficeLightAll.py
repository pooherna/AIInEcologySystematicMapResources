import pandas as pd
import matplotlib.pyplot as plt

# Load data
#df = pd.read_excel("Critical appraisal data extraction form (Responses).xlsx")
df = pd.read_excel("Critical appraisal data extraction form (Responses) March 12.xlsx")

# Remove metadata columns
drop_cols = [
    "Timestamp",
    "Extractor initials",
    "Study ID (format: first author_year_letterIfNeeded )",
    "DOI",
    "General note"
]

df = df.drop(columns=[c for c in drop_cols if c in df.columns])

# ----------------------------
# Short label mapping
# ----------------------------
label_map = {
"Provided citation, DOI or open access to published protocol": "Protocol available",
"Provided Boolean-style full search string and state the platform for which the string is formatted (e.g., Web of Science format)": "Full search string reported",
"The authors reported and visualised the review flow and the number of studies retained at each stage (e.g., in a PRISMA/ROSES diagram). ":"Provide PRSIMA/ROSES diagram",
"The authors described the data extraction process (who extracted, double-checking, and consistency)":"Data extraction process described",
"The authors reported using non-English search terms or provided search strings formatted for non-English databases as part of their search strategy.":"Reported Non-english terms used",
"If the authors explicitly stated that they intended to include any studies in languages other than English in their final dataset, select “Yes”.": "Intended to included Non-English reviews",
"The authors reported which languages were included in the literature search":"Languages searched reported",
"Described the process by which the comprehensiveness of the search strategy was assessed (i.e., list of benchmark articles)": "Search comprehensiveness assessed",
"Described the methodology for screening articles/studies for relevance. Methods for consistency of screening decisions (at title, abstract, and full texts levels) checking must be described.": "Screening method described",
"Described the review process, including the volume of evidence identified": "Review flow described",
"Provided the number of articles retained following full text screening": "Full-text inclusion counts",
"Provided a file and/or table containing raw extracted quantitative or qualitative data (study findings) from included studies": "Raw data provided",
"Provided metadata (i.e. detailed description of extracted raw variables) in a separate table, list and/or file for included studies": "Metadata provided",
"The authors provided analysis and/or figure-generation computer code/scripts.": "Code provided",
"Described knowledge gaps": "Knowledge gaps discussed",
"Provided any supplementary files": "Supplementary files provided",
"Described any financial or non-financial competing interests that the review authors may have or provided a statement of the absence of any potential competing interests": "Competing interests reported",
"The authors provided bibliographic information (for example, author, year, title, DOI) for all studies included in the synthesis.": "Included reviews listed",
"The authors provided bibliographic information for studies excluded at the full-text screening stage, including reasons for exclusion.": "Excluded reviews listed",
"The authors indicated whether they considered non-English studies during the screening process":"Non-English reviews considered"
}

# Apply label shortening
df = df.rename(columns=label_map)

# ----------------------------
# Convert answers to categories
# ----------------------------
def classify(v):
    if pd.isna(v):
        return "NA"
    v = str(v).lower()
    if v.startswith("yes"):
        return "Yes"
    if v.startswith("part"):
        return "Partially"
    if v.startswith("no"):
        return "No"
    return "NA"

summary = {}

for col in df.columns:
    vals = df[col].apply(classify).value_counts()
    summary[col] = vals

traffic = pd.DataFrame(summary).T.fillna(0)[["Yes","Partially","No","NA"]]

print(traffic)

# ----------------------------
# Plot
# ----------------------------
plt.figure(figsize=(9,8))

colors = {
"Yes":"green",
"Partially":"gold",
"No":"red",
"NA":"grey"
}

left = [0]*len(traffic)

for k in ["Yes","Partially","No","NA"]:
    plt.barh(traffic.index, traffic[k], left=left, color=colors[k], label=k)
    left = [l+v for l,v in zip(left, traffic[k])]

plt.xlabel("Number of reviews")
plt.title("Critical Appraisal of Reporting Quality")
plt.legend(title="Assessment")

plt.tight_layout()

plt.savefig("criticalTrafficLight.png", dpi=600, bbox_inches="tight")

plt.show()
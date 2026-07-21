from sdmetrics.reports.single_table import QualityReport
from synthetic_data_creation import train_df, synthetic_tvae, synthetic_ctgan, metadata

report = QualityReport()

report.generate(
    real_data=train_df,
    synthetic_data=synthetic_tvae,
    metadata=metadata.to_dict()
)

print(report.get_score())

print("\n**\n**\n")

report2 = QualityReport()

report2.generate(
    real_data=train_df,
    synthetic_data=synthetic_ctgan,
    metadata=metadata.to_dict()
)

print(report2.get_score())
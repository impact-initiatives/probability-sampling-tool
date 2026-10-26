One Excel file, `PST_Sampling_Output_<date>-<time>.xlsx`, with the **time of the draw in UTC**. It is made for the draw on screen: its **inputs** are the settings that the draw used, not those the form shows after it, so the file can be shared for validation.

| Sheet | Content |
|---|---|
| **{{sheet:inputs}}** | what is needed to reproduce the sample, in four sections: *Traceability* (version of the app, version of R, date and time of the draw in UTC), *Data source* (name of the file, its rows and columns), *Sampling* (mode, type, whether it is stratified and the columns chosen) and *Parameters* (those the mode and the type use, and the seed) |
| **{{sheet:summary}}** | the Sample summary table |
| **{{sheet:sample}}** | the Sample table |
| **{{sheet:frame}}** | the sampling frame as the tool read it: the file that was sampled, without the columns the tool adds |

The same file, the same settings, the same seed and the same version of the app give the same sample: to draw it again, save the sheet **{{sheet:frame}}** as a CSV file and load it in the tool.

An Excel sheet holds at most {{EXCEL_MAX_ROWS}} rows and {{EXCEL_MAX_COLUMNS}} columns: a larger sample is blocked when Sample is clicked, with a message that says what to reduce.

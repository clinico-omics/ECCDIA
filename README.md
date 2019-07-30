## `ECCDIA`
### ECCDIA Description
ECCDIA: an interactive web tool for comprehensive clinical and survival data analysis of esophageal cancer  (r version). The Esophageal Cancer Clinical Data Interactive Analysis (ECCDIA, <http://47.103.50.107:3838/ECCDIA/>) was developed to provide basic data analysis, survival analysis, and nomogram of the overall group and subgroup of 77,273 EC patients. The basic data analysis contained clinical factor ratio distribution analysis, Sankey plot analysis for relationships between clinical factors, and a map of clinical factors’ distribution. The survival analysis included Kaplan-Meier (K-M) analysis and Cox analysis for different subgroups of EC patients. The nomogram module enabled clinicians to precisely predict different subgroups of EC patients’ survival probability.

### parameter description

#### summary

Show 10/25/50/100: 改变显示条目数量

Clinical_Factor: 显示了所有可供选择的Clinical Factors

Group: Clinical Factor下相应的条目

No. Patients: 相应Group下的病例数目

Search: 您可以搜索相关关键词包括Clinical Factor/Group 

CLICK THE FIRST LINE: 颠倒显示顺序

#### clinical ratio

Ratio/Num：更改右图中频数分布图显示模式为Ratio/Number

Clinical Factors: 选择您需要分析其分布的Clinical Factor

Choose subgroup of data: 选择您需要分析的Clinical Factor在某一特定Subgroup中的分布，例：通过设定subgroup为[Sex，Female]，您可以选择只包含女性的数据

Legend Position: 选择图例标签显示在频数分布图的右侧(v)，或左下方(h)

X&Y tick size: 更改X轴及Y轴上的数字、字母字符大小(11-30)

X&Y Title font: 更改X轴及Y轴上的标题字符大小(11-30)

Legend lable size: 更改图例标签及文字大小(11-30)

Margin: 更改分布图周围空白边缘大小

#### sankey plot

Horizontal or vertical: 更改sankey plot为水平/垂直显示

Clinical factors: 选择需要绘制sankey plot进行分析的clinical factors(可选择2-3个clinical factors)

Choose subgroup of data: 选择您需要分析的clinical factors在特定的subgroup中的分布

Lable size: 更改lable字符大小

#### survival rate

OS or CSS：选择需要分析Overall Survival(OS) 或Cancer Specific Survival(CSS)

Survival rate: 选择需要分析的生存时长(1-10年)

Choose clincal factor: 选择需要显示在survival rate图上的clinical factors and subgroups

Legend Position: 选择图例标签显示在频数分布图的右侧(v)，或左下方(h)

Line size：改变生存率曲线的粗细

X&Y tick size: 更改X轴及Y轴上的数字、字母字符大小(11-30)

X&Y title font: 更改X轴及Y轴上的标题字符大小(11-30)

Legend lable size: 更改图例标签及文字大小(11-30)

Margin: 更改分布图周围空白边缘大小

#### KM Analysis

**Common parameters:**

OS or CSS：选择需要分析Overall Survival(OS) 或Cancer Specific Survival(CSS)的KM Analysis

Clincal factor: 选择需要进行KM Analysis的clinical factors 

Choose subgroup of data: 选择需要进行KM Analysis的subgroup

**Static parameters:**

Survival median line: 选择survival median line的显示为none/hv/h/v

Risk table: 勾选更改是否显示Risk table

Necesor plot: 勾选更改是否显示Necesor plot

Main fontsize (title/x/y/ticks): 更改KM analysis中主要字体大小包括标题、xy轴、标记(15-40)

Table fontsize: 更改risk table上的字符大小(5-30)

Ncensor plot height: 更改Ncensor plot的高度

Line size: 改变线的粗细

**Dynamic parameters:**

Risk table:勾选更改是否显示Risk table

Main fontsize (title/x/y/ticks): 更改dynamic survival analysis中主要字体大小包括标题、xy轴、标记(15-40)

Table fontsize: 更改risk table上的字符大小(5-30)

Table height: 更改risk table的高度(0.1-1)

Line width: 改变线的粗细

#### cox analysis

OS or CSS：选择需要分析Overall Survival(OS) 或Cancer Specific Survival(CSS)的cox analysis

univariate or multivariate: 选择需要进行univariate/multivariate analysis

cox subgroup: 选择需要进行cos analysis的subgroup

#### nomogram

OS or CSS：选择需要分析Overall Survival(OS) 或Cancer Specific Survival(CSS)的nomogram

Nomogram subgroup: 选择需要分析的subgroup，不同的subgroup参与nomogram预测模型的临床参数会发生改变

#### map

Clincal factor: 能够在map上面动态出现的临床参数信息

###  Abberation table

| Clinical Factor (raw name) | Clinical Factor (raw)                                        | Clinical Factor (redefine) | Clinical Factor (redefine name)                              |
| -------------------------- | ------------------------------------------------------------ | -------------------------- | ------------------------------------------------------------ |
| Sex                        | Male                                                         | Male                       | Sex                                                          |
|                            | Female                                                       | Female                     |                                                              |
| Primary Site               | C15.0-Cervical esophagus                                     | CE                         | Primary_Site                                                 |
|                            | C15.1-Thoracic esophagus                                     | TE                         |                                                              |
|                            | C15.2-Abdominal esophagus                                    | AE                         |                                                              |
|                            | C15.3-Upper third of esophagus                               | UTE                        |                                                              |
|                            | C15.4-Middle third of esophagus                              | MTE                        |                                                              |
|                            | C15.5-Lower third of esophagus                               | LTE                        |                                                              |
|                            | C15.8-Overlapping lesion of esophagus                        | OLE                        |                                                              |
|                            | C15.9-Esophagus, NOS                                         | NOS_E                      |                                                              |
| Grade                      | Well differentiated; Grade I                                 | I                          |                                                              |
|                            | Moderately differentiated; Grade II                          | II                         |                                                              |
|                            | Poorly differentiated; Grade III                             | III                        |                                                              |
|                            | Undifferentiated; anaplastic; Grade IV                       | IV                         |                                                              |
|                            | Unknown                                                      | Unknown                    |                                                              |
| Histologic type            | 8050-8089: squamous cell neoplasms                           | SCC                        | Histologic_type                                              |
|                            | 8140-8389: adenomas and adenocarcinomas                      | AD                         |                                                              |
| Stage 7th                  | IA                                                           | IA                         | Stage_9_group                                                |
|                            | IB                                                           | IB                         |                                                              |
|                            | IIA                                                          | IIA                        |                                                              |
|                            | IIB                                                          | IIB                        |                                                              |
|                            | IIIA                                                         | IIIA                       |                                                              |
|                            | IIIB                                                         | IIIB                       |                                                              |
|                            | IIIC                                                         | IIIC                       |                                                              |
|                            | IIINOS                                                       | IIINOS                     |                                                              |
|                            | IV                                                           | IV                         |                                                              |
|                            | Unkonwn                                                      | Unkonwn                    |                                                              |
| Stage 7th                  | IA/B                                                         | I                          | Stage_4_group                                                |
|                            | IIA/B                                                        | II                         |                                                              |
|                            | IIIA/B/C/NOS                                                 | II                         |                                                              |
|                            | IV                                                           | IV                         |                                                              |
|                            | Unknown                                                      | Unknown                    |                                                              |
| T stage 7th                | T0                                                           | T0                         | T_9_group                                                    |
|                            | T1a                                                          | T1a                        |                                                              |
|                            | T1b                                                          | T1b                        |                                                              |
|                            | T1NOS                                                        | T1NOS                      |                                                              |
|                            | T2                                                           | T2                         |                                                              |
|                            | T3                                                           | T3                         |                                                              |
|                            | T4a                                                          | T4a                        |                                                              |
|                            | T4b                                                          | T4b                        |                                                              |
|                            | T4NOS                                                        | T4NOS                      |                                                              |
|                            | Tx                                                           | Tx                         |                                                              |
| T stage 7th                | T0                                                           | T0                         | T_5_group                                                    |
|                            | T1a/b/NOS                                                    | T1                         |                                                              |
|                            | T2                                                           | T2                         |                                                              |
|                            | T3                                                           | T3                         |                                                              |
|                            | T4a/b/NOS                                                    | T4                         |                                                              |
|                            | Unknown                                                      | Unknown                    |                                                              |
| N stage 7th                | N0                                                           | N0                         | N_4_group                                                    |
|                            | N1                                                           | N1                         |                                                              |
|                            | N2                                                           | N2                         |                                                              |
|                            | N3                                                           | N3                         |                                                              |
|                            | NX                                                           | Unknown                    |                                                              |
| N stage 7th                | N0                                                           | N0                         | N_2_group                                                    |
|                            | N1/2/3                                                       | N1_3                       |                                                              |
|                            | NX                                                           | Unknown                    |                                                              |
| M stage 7th                | M0                                                           | M0                         | M                                                            |
|                            | M1                                                           | M1                         |                                                              |
|                            | Unknown                                                      |                            |                                                              |
| Surgery of Primary Site    | Local tumor destruction                                      | LTD                        | Surgery_of_Primary_Site                                      |
|                            | Local tumor excision                                         | LTE                        |                                                              |
|                            | Esophagectomy                                                | Esophagectomy              |                                                              |
|                            | no surgery                                                   | no_surg                    |                                                              |
|                            | Surgery NOS                                                  | Unknown                    |                                                              |
|                            | Unknown                                                      | Unknown                    |                                                              |
| Radiation recode           | Beam radiation                                               | Yes                        | Radiation_recode                                             |
|                            | Combination of beam with implants or isotopes                | Yes                        |                                                              |
|                            | Other than beam radiation (1973-1987 cases only)             | Yes                        |                                                              |
|                            | Radiation, NOS  method or source not specified               | Yes                        |                                                              |
|                            | Radioactive implants (includes brachytherapy) (1988+)        | Yes                        |                                                              |
|                            | Radioisotopes (1988+)                                        | Yes                        |                                                              |
|                            | Recommended, unknown if administered                         | Unknown                    |                                                              |
|                            | Refused (1988+)                                              | No                         |                                                              |
|                            | None/Unknown                                                 | Unknown                    |                                                              |
| Chemotherapy               | Yes                                                          | Yes                        | Chemotherapy                                                 |
|                            | No/Unknown                                                   | No_or_Unknown              |                                                              |
| Therapy method             | Surgery (Yes) + Radiation (Yes) + Chemotherapy (Yes)         | Surg_Rad_Che               | Therapy_method (based on surgey, radiation and Chemotherapy) |
|                            | Surgery (Yes) + Radiation (No) + Chemotherapy (Yes)          | Surg_Che                   |                                                              |
|                            | Surgery (No) + Radiation (Yes) + Chemotherapy (Yes)          | Rad_Che                    |                                                              |
|                            | Surgery (No) + Radiation (No) + Chemotherapy (Yes)           | Only_Che                   |                                                              |
|                            | Surgery (Yes) + Radiation (Yes) + Chemotherapy(No_or_Unknown) | Almost_Surg_Rad            |                                                              |
|                            | Surgery (Yes) + Radiation (No) + Chemotherapy(No_or_Unknown) | Almost_Only_Surg           |                                                              |
|                            | Surgery (No) + Radiation (Yes) + Chemotherapy(No_or_Unknown) | Almost_Only_Rad            |                                                              |
|                            | Surgery (No) + Radiation (No) + Chemotherapy(No_or_Unknown)  | Almost_no_therapy          |                                                              |
|                            | Unknown                                                      | Unknown                    |                                                              |



### Author(s)
Jun Shang (shangjunv@163.com)
# AUTHFRDS specs (second playbook target)

Who produced what:
| Path | Produced by |
|---|---|
| `BUSINESS-RULES.md`, `DATA-MODEL.md`, `COPAUS2C.md`, `COPAUS1C.md`, `GAP-REVIEW.md` | **Devin** (hand-written in AWS Transform's layout; not AWS Transform output) |
| `aws-transform-output/` | **AWS Transform for mainframe** - downloaded from the job's Artifacts and committed unchanged (see its README) |

AWS Transform job: workspace `CardDemo-Transform-Demo` (account 374199896615, us-east-1), job `MainframeModernization-2026-10-29-1806` (id `e57e66bc-5dba-49e6-85ee-c7a5bed07ffc`), input `s3://transform-demo-carddemo-374199896615/input/carddemo-authorization-ims-db2-mq.zip` (tag `Project=transform-demo`). Steps requested: Analyze code, Generate technical documentation, Extract business logic.

Scope: `ddl/AUTHFRDS.ddl`, `ddl/XAUTHFRD.ddl`, `dcl/AUTHFRDS.dcl`, `cbl/COPAUS2C.cbl`, `cpy/CIPAUDTY.cpy`, and the COPAUS1C PF5/PF8/ENTER paths. IMS and MQ are stubbed.

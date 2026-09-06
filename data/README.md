# Data snapshot

These files are committed so the coursework dashboard can be reproduced
without downloading changing upstream data.

| File | Role | SHA-256 |
| --- | --- | --- |
| `owid-energy-data.csv` | Country-year energy indicators used by the app | `64a3276e21703577bb4e4cd11d0dae4aaa2b94aaacc4132d9b8f0c1495c74e2c` |
| `owid-energy-codebook.csv` | Definitions, units, and underlying sources for OWID fields | `0ec916e30371d892ad189432f28f9ce8dd1ca68d494b780f910e5ee24274d139` |
| `world-countries.json` | Country ISO-3 and UN-region attributes used for grouping | `6c00385b605c80f09441d69cbb3b2bfa6dde01d1d65fe7f285499e37a505cda9` |

The OWID snapshot contains records through 2024, with incomplete and
indicator-specific coverage. The original coursework repository did not record
the precise upstream commit or retrieval time. The boundary file has a Natural
Earth-derived schema, but its original URL and release were also not recorded.
These provenance limits are retained explicitly rather than reconstructed from
guesswork.

See the repository README for source links, interpretation limits, and licence
notes.

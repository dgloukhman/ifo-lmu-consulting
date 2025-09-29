## Turning Point Indication Analysis

This section describes the scripts used for identifying leading indicators for business cycle turning points. The main goal is to find sub-series from the IFO survey that can predict turning points in the main manufacturing business climate index in real-time.

- `TurningPointMain.R`: This is the main script for the analysis. It identifies turning points in the main index using the Bry-Boschan algorithm as a benchmark. Then, it computes real-time turning point signals for sub-series using Markov-Switching models. It evaluates individual sub-series and also groups of sub-series to create a composite indicator, which is then evaluated.
- `turningpoint_utils.R`: Contains all the core utility functions for the turning point analysis. This includes functions for the Bry-Boschan algorithm, Markov-Switching models, signal evaluation, and grouping of series.
- `TP_UnusedStuff/`: This directory contains various scripts from earlier, exploratory stages of the analysis. They are not part of the final analysis but are kept for reference.

Run Order:
The analysis can be run via `TurningPointMain.` Running the function `get_markov_probabilities()` to fit Markov Switching models for all sorts of subseries might take some time. For a simple quick run, it is recommended to obtain the Markov probabilities straight away from `Data/turningpoint_intermediate/ifo_probs_two_digits.csv`. 
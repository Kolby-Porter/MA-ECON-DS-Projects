import numpy as np
import pandas as pd

def build_constraints(tickers, portfolio_groups, asset_class_caps):
    ticker_asset_class = {
        ticker: group
        for group, tickers_in_group in portfolio_groups.items()
        for ticker in tickers_in_group
    }

    asset_class = pd.Series(tickers).map(ticker_asset_class)

    constraints = [{"type": "eq", "fun": lambda w: np.sum(w) - 1}]

    for group, cap in asset_class_caps.items():
        mask = (asset_class == group).astype(float).values
        constraints.append({
            "type": "ineq",
            "fun": lambda w, cap=cap, mask=mask: cap - np.dot(w, mask)
        })

    return constraints
    
import numpy as np

portfolio_groups = {
    "Dom_Eq": ['SPY', 'QQQ', 'IWM'],
    "Intl_Eq": ['EFA', 'EEM'],
    "Fixed_Inc": ['AGG', 'TLT', 'LQD'],
    "Alt": ['GLD', 'VNQ'],
    "Factor": ['MTUM', 'VLUE', 'QUAL', 'USMV']
}

asset_class_caps = {
    "Alt": 0.15,
    "Fixed_Inc": 0.30,
    "Dom_Eq": 0.20,
    "Intl_Eq": 0.15,
    "Factor": 0.20
}

asset_bounds = (0, 0.15)


import numpy as np

def portfolio_return(weights, returns):
    return np.dot(weights, returns)

def portfolio_volatility(weights, cov_matrix):
    return np.sqrt(weights.T @ cov_matrix @ weights)

def sharpe_ratio(weights, returns, cov_matrix, rf_annual_rate):
    port_returns = portfolio_return(weights, returns)
    port_volatility = portfolio_volatility(weights, cov_matrix)
    return (port_returns - rf_annual_rate) / port_volatility
def negative_sharpe(weights, returns, cov_matrix, rf):
    return -sharpe_ratio(weights, returns, cov_matrix, rf)

def portfolio_beta(portfolio_return, benchmark_return):
    #beta = cov / benchmark_var
    beta = benchmark_return.cov(portfolio_return) / benchmark_return.var()
    return beta
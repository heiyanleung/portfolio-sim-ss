# portfolio-sim-ss
Single Index Model with Short Sales

Once we have a return data frame or matrix set up, similar to the Equal Allocation
Portfolio case above, we can easily calculate our Single Index Model optimal
portfolio weights, return, and risk using the port.sim.ss() function if short
sales are allowed.  For example:

port.sim.ss(data=ret.mon,idx=dji.mon,rf=0.001,lint=TRUE)

where ret.mon is the name of my data frame or matrix with monthly returns of
each stock in each column, dji.mon is the market return data (in this case we
use the DJIA index as our market), rf is the risk free rate (in this case we use the
3 year treasury bill rate published on www.finance.yahoo.com), and lint is the
logical argument of whether we want to apply the Lintner’s definition of short
sales. 

# 4. Suppose you are rolling two fair dies with success defined as getting a total value 6. If you roll two dies independently for eight times:
# 4.1 What is the probability of observing exactly five successes (five total value 6s) in total? (calculated by hand) 
# 4.2 Use R to confirm the result of Pr(X=5) for the die-roll example. 
# 4.3 Plot the corresponding full probability mass function for X for this die-rolling example.

########### Q4 Part 1 ########### 
# Success: Rolling 2 die 8 times, and getting 6 as sum, 5 times out of 8
# sum =6 -> (1,5),(5,1),(2,4),(4,2),(3,3)
# p, prob(sum=6) = (5/36)
# q, prob(sum!=6) =  (31/36)
# n, no of trials = 8
# x, no of success = 5

# Pobability of Success
# p(X=x) = nCx * p^x * q^(n-x)
#        = 8C5 * (5/36)^5 * (31/36)^3
choose(8,5) * (5/36)^5 * (31/36)^3
# result = 0.001848005

########### Q4 Part 2 ########### 
# Using R Function:
dbinom(5,8,5/36)
# result = 0.001848005
# here no of success = 5, total trials = 8, probability of success = 5/36

########### Q4 Part 3 ########### 
prob<-dbinom(5,8,5/36)
trials<-c(1:8)
plot(trials, dbinom(trials, size=8, prob=5/36),type='h',xlab = 'Trials', ylab = 'Probability', main='Probability Mass Function')

############### Define parameters ########################

#number of simulations
Nsim = 1000

#number of agents in each simulation
Nagent = 200

#Agents Distribution
#pyramid w/ 20% lv0
Da = function(n) {
	sample(c(0, 1, 2, 3, 4), size = n, prob = c(0.2, 0.32, 0.24, 0.16, 0.08), replace = T)
	}
	#large_middle
		#s = sum(pnorm(-2)*2+pnorm(-1)*2+pnorm(0))
		#Da = function(n) {
			#sample(c(0, 1, 2, 3, 4), size = n, 
			#prob = c(pnorm(-2)/s, pnorm(-1)/s, pnorm(0)/s, pnorm(-1)/s, pnorm(-2)/s), replace = T)}
	#uniform
		#Da = function(n) {sample(c(0, 1, 2, 3, 4), size = n, replace = T)}
	#upside-down pyramid
		#Da = function(n) {
			#sample(c(0, 1, 2, 3, 4), size = n, prob = c(0.2, 0.08, 0.16, 0.24, 0.32), replace = T)}
	
#number of Qs in the quesiton pull
Nqt = 100

#number of question in each round (need to be divisible of Nqt)
Nqr = 4

#Question distribution
#pyramid
Dq = function(n) {
	sample(c(1, 2, 3, 4), size = n, prob = c(0.4, 0.3, 0.2, 0.1), replace = T)
	}
	#large_middle
		#d = pnorm(2)-pnorm(-2)
		#Dq = function(n) {
			#sample(c(1, 2, 3, 4), size = n, 
			#prob = c((pnorm(-1)-pnorm(-2))/d, (pnorm(0)-pnorm(-1))/d, (pnorm(1)-pnorm(0))/d, (pnorm(2)-pnorm(1))/d), replace = T)}
	#uniform
		#Dq = function(n) {sample(c(1, 2, 3, 4), size = n, replace = T)}
	#upside-down pyramid
		#Dq = function(n) {
			#sample(c(1, 2, 3, 4), size = n, prob = c(0.1, 0.2, 0.3, 0.4), replace = T)}
	
##Current score function: naiive. 
	#Reward money divided by all majority voters (considered right answer)
	#Minority (considered wrong answer) receive a negative pay = -1/3 of the correct pay to make the expected pay of random guess zero

#define probability of answering right for each level
qlevel = data.frame(level = c (1, 2, 3, 4))
prob_1 = data.frame (prob_1 = c(1, 1/2, 1/3, 1/4))
prob_2 = data.frame (prob_2 = c(1, 1, 1/2, 1/3))
prob_3 = data.frame (prob_3 = c(1, 1, 1, 1/2))
prob_4 = data.frame (prob_4 = rep(1, 4))
prob_0 = data.frame (prob_0 = c(1/2, 1/3, 1/4, 1/4))
prob = cbind (qlevel, prob_0, prob_1, prob_2, prob_3, prob_4)

################### simulation part ####################
for (i in 1:Nsim) {
	### In each simulation

	#Draw agent sample
	agents = Da(Nagent)

	#Draw question pool, create answered_by column
	q_pool = Dq(Nqt)
	answered_by = rep(0, Nqt)
	q_pool = as.data.frame(cbind(q_pool, answered_by))
	#Keep track of round
	q_pool$round = rep(1:(Nqt/Nqr), each = Nqr)

	#Questions are later given in each round by order in q_pool

	#Assumption: agents are able to answer the question at or below their levels 100% right; 
		#then 50%; 33% and 25% for the levels up 
		#(multiple choice, ability = ability to rule out wrong answers)
		#further step, may add error term

	##First round, everyone answers the question that matches their level
	
	#create table for agent counts at each level
	level = data.frame(level = c (1, 2, 3, 4))
	freq = data.frame(freq = tabulate(agents, nbins = 4))
	agent_freq = as.data.frame(freq)
	agent_freq = cbind(level, agent_freq)

	#create table for question counts at each level for the first 4 Qs
	freq = tabulate(q_pool$q_pool[1:4], nbins = 4)	
	q_freq = as.data.frame(freq)
	q_freq = cbind(level, q_freq)

	#calculate how many agnet will be answering each question
	for (i in 1:4) {
		if (freq[i] == 0) {
			q_freq$answered_by[i] = 0
		} else {
		q_freq$answered_by[i] = agent_freq$freq[i]/q_freq$freq[i]
		}

		# map that back to q_pool
		q_pool$answered_by[i] = q_freq$answered_by[i]
	############ this part may change if change payment scheme############
	#calculate payment for each level of question
	#currently, set payment for each question (now using 100) evenly distributed to agents
		if (q_pool$answered_by[i] == 0) {
			q_pool$payment[i] = 0
		} else {
		q_pool$payment[i] = 100/q_pool$answered_by[i]
		}
	#Note that after the payment is shown to agents, they should be able to calculate population distribution
	############ end of this subject-to-change part############
	}

	#record current question matrix for next round use
	q_previous = q_freq
	q_previous$payment = q_pool$payment[1:4]
	

###@####### some ideas: update a matrix w/ agents level and difficulty level,
### add last round payment, get number of agents trying each level of questions for the round
### calc probability (proportional to expected payment)
### sample each agent (from the count matrix) by their probability accordingly

	#Reward is set to be 100 for each question. Divided according to score function
		#currently, divided evenly among all people answered it right (majority)

	#Payment for this question is told to agents.
	#Agents calculate expected pay for question difficulties. 
		#Use mixed strategy to decide if swich or not
		#Assumption: probability only based on previous round
######## end of some ideas#########

	#Rest of the rounds, need to go by agents
	for (j in 2:Nqt/Nqr) {
		#First get distribution of questions
		freq = tabulate(q_pool$q_pool[((j-1)*Nqr+1):j*Nqr], nbins = 4)	
		q_freq = as.data.frame(freq)
		q_freq = cbind(level, q_freq)

		#Calculate expected payment for right answer based on previous Q & current Q dist.
		q_freq$exp.pay.single = q_previous$payment/q_previous$freq*q_freq$freq
		for (i in 1:4){
			# for questions that didn't appear in previous round, calculated expected payment as if all agent in the according level will answer this question
			if (q_freq$exp.pay.single[i] == Inf) {
				q_freq$exp.pay.single[i] = 100/agent_freq$freq[i]
			}
		}
		
		for (k in 1:Nagent) {
		#Calculate expected payment, note that agents know population distribution now
		#Although currently not considering population dist.
		#Calculated only based on agent level and previous payment (and how many Qs in that level previously)
		q_freq$exp.pay.round = q_freq$exp.pay.single * prob[[(agents[k]+2)]]


		#answer questions based on strategy, record in matrix
		}
	#After all agents answered the questions...
	#Calculate payment, go to next round
	}

}

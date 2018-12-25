
beta = c(0,0)
phi = 0
f = function(x, z){return(-2 + t(beta) %*% x + phi * z)}
x1 = rnorm(100)
x2 = rnorm(100)

t=1
voices = rbinom(1,1,
                exp(f(c(x1[t],x2[t]),.5))/
                  (1+exp(f(c(x1[t],x2[t]),.5))))
for(t in 2:100){
  voices = append(voices, rbinom(1,1,exp(f(c(x1[t],x2[t]),voices[t-1]))/
                                   (1+exp(f(c(x1[t],x2[t]),voices[t-1])))))
}
data = data.frame(voices,x1,x2)
glm(voices ~ x1 + x2 + 0)

fake_model = brm(voices ~ x1 + x2, data = data,  family = bernoulli(link = "logit"), chains = 1)
# D function
# Takes two functions as input: 
# function 1: f a function of 2 variables 
# and function 2: r a function of 1 variable, preferably one that describes a sequence.
# Returns a function of 1 variable that describes a sequence.

d <- function (f, r)
{
  #Describe function y, to be returned.
  y<-function(x)
  {
    z<-c()
    z1<-1:x
    i<-1
    j<-1
    
    #Iterate up to the term we want.
    for(derp in z1)
    {
      #Catch the case that an r doesn't exist.
      if(is.nan(r(i)) | is.na(r(i)) )
      {
        z<-c(z, 0)
        i<-(i+1)
      }
      else
      {
        #starting with j at 1 unril the i'th r term, we want to return f(i, j)
        if(j<r(i))
        {
          z<-c(z, f(i, j))
          j<-(j+1)
        }
        else # j = r(i) and we want to return that term but also reset j and increment i.
        {
          z<-c(z, f(i, j))
          j<-1
          i<-(i+1)
        }
      }
    }
    return(z[x])
  }
  
  return(y)
}

#--------------------------------------------------------------------------
#Some test cases

f<- function(a,b){a+b} #My Test f
r<- function(a){a**3} #My test r

sapply(1:20, d(f,r)) #applies the function returned by d(f,r) across the numbers 1 through 20.



f = function(x,y){   #Liam's Test f
  return(x + 2*y)
}
r <- function(n){    #Liam's Test r
  a <- NaN    #NaN = Not a number. Bound so r isn't infinite when testing
  if(n < 5){
    a <- 2*n - 1
  }
  return(a)
}

sapply(1:20, d(f,r)) #applies the function returned by d(f,r) across the numbers 1 through 20.
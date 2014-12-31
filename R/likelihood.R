likelihood <-
function(po, ne, lamda, thres)
{
return (log(1-ppois(thres-1, lamda))*po+log(ppois(thres-1, lamda))*ne) 
}

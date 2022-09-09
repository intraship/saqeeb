#4 for Visa cards
#5 for Master cards
#37 for American Express cards
#6 for Discover cards

# Luhn algorithm if it's a valid card #
install.packages(stringr)
library(stringr)

creditCard1="379354508162306"
creditCard2="4388576018402626"

cardSerial1=strsplit(creditCard1,"")
cardSerial2=strsplit(creditCard2,"")

cardArray1=unlist(cardSerial1)
cardArray2=unlist(cardSerial2)

creditArray=function(creditCard){
  unlist(strsplit(creditCard,""))
}


typeOfCard=function(cardNo){
  if (cardNo[1]=='4'){
    print("might be visa, continue to check")
    return (TRUE)
  } else if (cardNo[1]=='5'){
    print("might be master, continue to check")
    return (TRUE)
  } else if (cardNo[1]=='6'){
    print("might be Discover, continue to check")
    return (TRUE)
  } else if (cardNo[1]=='3'&cardNo[2]=='7'){
    print("might be American Express, continue to check")
    return (TRUE)
  } else{
    print("this is a invalid card type, quit!")
    return (FALSE)
  }
}

LuhnCheckSum=function(cardNo) {
  checksum=0
  cardLen=length(cardNo)
  for (i in 1:cardLen){
    if(i%%2==1) {
      checksum=checksum+as.numeric(cardNo[cardLen-i+1])
    }else{
    evensum=as.numeric(cardNo[cardLen-i+1])*2;
    checksum=checksum+(evensum%/%10+evensum%%10)
  }
  }
  return (checksum)
}


cardValidation=function(creditCard){
  cardArray=creditArray(creditCard)
if (typeOfCard(cardArray)==TRUE){
  checksum=LuhnCheckSum(cardArray)
  if (checksum%%10==0){
    print("OK, a valid card!")
    return (TRUE)
  }else{
    print("invalid card checksum, quit!")
    return (FALSE)
  }
   
}else{
   return (FALSE)
}
  
}

cardValidation(creditCard1)

cardValidation(creditCard2)

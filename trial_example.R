x <- c(9:20, 1:5, 3:7, 0:8)
# ## extract unique elements

(xu <- x[!duplicated(x)])
# ## similar, same elements but different order:
(xu2 <- x[!duplicated(x, fromLast = TRUE)])

(xu3 >- x[!duplicated(x)|!duplicated(x, fromLast=TRUE)])
(xu4 >- x[!duplicated(x)|!duplicated(x, fromLast=FALSE)])
x[!duplicated(x)&!duplicated(x, fromLast=TRUE)]
#
# ## xu == unique(x) but unique(x) is more efficient
stopifnot(identical(xu,  unique(x) |rev(unique(x)),
identical(xu2, unique(x, fromLast = TRUE))))
stop(identical(xu,  unique(x)),
identical(xu2, unique(x, fromLast = TRUE)))
?stop
?identical

x <- c(9:20, 1:5, 3:7, 0:8)
duplicated(x)|rev(duplicated(rev(x)))
xu1 <- x[!duplicated(x)& rev(!duplicated(rev(x)))]

x <- c(9:20, 1:5, 3:7, 0:8)
unique(x)

#table(Full_titanic$Fare,Full_titanic$Survived)

#unique(Full_titanic$Ticket)
#DTicket <- Full_titanic[unique(Full_titanic$Ticket),]
#NDTicket <- Full_titanic[duplicated(Full_titanic$Ticket),]
#cabinreturn <- Full_titanic[grepl(paste(x, collapse = "|"), Full_titanic$Cabin),]


#table(Full_titanic$Ticket[unique(Full_titanic$Ticket)],Full_titanic$Survived)
#Full_titanic$Ticket[duplicated(Full_titanic$Ticket)]
#sum(table(NDTicket$Ticket))
#nrow(NDTicket)
#sum(table(NDTicket$Survived==1))
#table(NDTicket$Ticket,NDTicket$Survived==1)


# ticket.unique <- rep(0, nrow(Full_titanic))
# tickets <- unique(Full_titanic$Ticket)
# length(tickets)
# 
# for (i in 1:length(tickets)) {
#   current.ticket <- tickets[i]
#   party.indexes <- which(Full_titanic$Ticket == current.ticket)
#   for (k in 1:length(party.indexes)) {
#     ticket.unique[party.indexes[k]] <- length(party.indexes)
#   }
# }
# ticket.unique




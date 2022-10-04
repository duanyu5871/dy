\name{dy_as_numeric}
\alias{dy_as_numeric}
\title{Date Transform To Numeric}
\usage{
dy_as_numeric(x, allowN=1, allowBeginZero=T, ...)
}
\arguments{
\item{x}{A vector or data.frame or sth. }

\item{allowN}{Max n number for function to allow different prefix or suffix in a potential numeric vector}

\item{allowBeginZero}{Whether to change prefix 0 in number}

}

\description{
Data transformation
}

\examples{
# bad_data
print(bad_data)

# transformed bad_data
new_data<-dy_as_numeric(bad_data)
print(new_data)

# a aggressive parameter
new_data<-dy_as_numeric(bad_data,allowN=2)
print(new_data)
}

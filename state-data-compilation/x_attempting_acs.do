// https://gahwan.com/stata-getcensus-package-for-american-community-survey-datasets/
// https://centeronbudget.github.io/getcensus/syntax.html

net install getcensus
net set ado "T:\ado"
adopath ++ "T:\ado"

global censuskey 651e2de6f551da69e6759ddd8cd251a87ab3c7d8

getcensus catalog, year(2021) sample(5) product(DT)

getcensus catalog, search(cpi)
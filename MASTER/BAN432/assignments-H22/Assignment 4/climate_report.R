require(pdftools)
require(tidytext)
require(tidyverse)
require(quanteda)

load("ipcc.RData")

ipcc6 <- ipcc6[1:100]
ipcc5 <- ipcc5[1:100]

stopwords <-
  stopwords::stopwords()


toks6 <- tokens(ipcc6)
toks5 <- tokens(ipcc5)

kwic.token <- function(data, terms) {
  kwic(
    x=data,
    pattern = terms,
    valuetype = "regex",
    window = "4",
    case_insensitive =  T
  )
}

kw.ipcc5.modal <- kwic.token(ipcc5, termsModal)
kw.ipcc5.uncert <- kwic.token(ipcc5, termsUncertainty)
kw.ipcc6.modal <- kwic.token(ipcc6, termsModal)
kw.ipcc6.uncert <- kwic.token(ipcc6, termsUncertainty)


# IPCC5 Percentages
out <- convert(dfm(toks5), to = "data.frame")
ipcc5_df <- out %>% 
  pivot_longer(cols = c(!doc_id), names_to = "tokens", values_to = "freq") %>% 
  filter(!tokens == stopwords,
         nchar(tokens) > 2)

ipcc5.modal <- nrow(kw.ipcc5.modal)
ipcc5.uncert <- nrow(kw.ipcc5.uncert)
ipcc5.sum.tokens <- sum(ipcc5_df$freq)

# IPCC6 Percentages
out <- convert(dfm(toks6), to = "data.frame")
ipcc6_df <- out %>% 
  pivot_longer(cols = c(!doc_id), names_to = "tokens", values_to = "freq") %>% 
  filter(!tokens == stopwords,
         nchar(tokens) > 2)

ipcc6.modal <- nrow(kw.ipcc6.modal)
ipcc6.uncert <- nrow(kw.ipcc6.uncert)
ipcc6.sum.tokens <- sum(ipcc6_df$freq)

tibble(
  IPCC5_ModalRatio = ipcc5.modal/ipcc5.sum.tokens,
  IPCC5_UncertainRatio = ipcc5.uncert/ipcc5.sum.tokens,
  IPCC6_ModalRatio = ipcc6.modal/ipcc6.sum.tokens,
  IPCC6_UncertainRatio = ipcc6.uncert/ipcc6.sum.tokens
) -> output.percentages

output.percentages





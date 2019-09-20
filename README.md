# journalclub
Uses citations and PubMed queries to generate new publications for a journal club.

## Work-flow

```{r}
library(journalclub)
```

Retrieve papers published in the last 30 days that either cite
a previously presented paper or match the PubMed query.
Return any of these have not previously been either presented or rejected
for discussion.
```{r}
candidates <- journalclub.candidates("inputs", recent=30)
```

Have a look through the candidate papers and
then update the journal club history by providing the PMIDs for the
papers that will be discussed.
```{r}
journalclub.update("inputs", presented)
```




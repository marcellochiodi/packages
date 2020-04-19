
MLA.explor.pairs <-
function(x,...){
        pairs(x,...,upper.panel=panel.lmsm,lower.panel=panel.lmsm,diag.panel=panel.H1)
                            }

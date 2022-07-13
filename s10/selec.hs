--IN : Uma lista qualquer u e uma lista de posições P
--OUT: Lista das chaves de u cujas posições estão em P
selec u p = [u !! i | i <- p]
rotEsq n lista = (rotEsq (n-1) ((tail lista)++[(head lista)]))
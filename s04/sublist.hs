sublist begin' end' list = drop begin (take end list)
    where size = length list
          begin = if begin' < 0 then size + begin' else begin'
          end = if end' < 0 then size + end' else end'


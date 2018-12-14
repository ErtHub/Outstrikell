module OSSolver where

pinpointSubstr [] _ = error "Trying to pinpoint an empty substring!"
pinpointSubstr substr str = [x | x <- [0..(length str - length substr)], substr == crop str x (length substr)]

crop _ _ 0 = []
crop (c:str) 0 y = c:(crop str 0 (y - 1))
crop (c:str) x y = crop str (x - 1) y

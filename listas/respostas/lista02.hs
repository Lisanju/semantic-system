bmi :: Float -> Float -> Float
bmi w h = w / h^2

reportBMI :: Float -> Float -> String 
reportBMI w h | bmi w h < 18.5 = "underweight"
              | bmi w h <= 25  = "normal weight"
              | bmi w h <= 30  = "overweight"
              | otherwise      = "obese"

healthyRange :: Float -> (Float,Float) 
healthyRange h = (18.5 * h^2 , 25 * h^2)

bmiPrime :: Float -> Float -> Float
bmiPrime w h = bmi w h / 25

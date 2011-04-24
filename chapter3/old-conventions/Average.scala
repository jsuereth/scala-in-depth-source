object Average {


   def avg(values : List[Double])(implicit adjustment : Double => Double) = {
     val sum = values.foldLeft(0.0) {  
       (sum, cur) => sum + adjustment(cur)
     }
     sum / values.size.toDouble
   }

   def stdDev(values : List[Double]) = {
     val avgVal = avg(values)
     avg(values) 
     { 
         x : Double => Math.abs(x - avgVal) 
     }
   }
}

object Average {
   def avg(values : List[Double] = List(0.0,1.0,0.5)) = {
     val sum = values.foldLeft(0.0) { _ + _ }
     sum / values.size.toDouble
   }

   def `avg$default$1` = List(0.0,0.0,0.)

   /*class `$anonfun` {
      class `1` {
        println("O MY!")
      }
   }*/
}

/*class `Average$$anonfun$1` {
   println("O MY!")
}*/


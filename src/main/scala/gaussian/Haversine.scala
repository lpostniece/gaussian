package gaussian

import gaussian.Gaussian.Location

import math._

object Haversine {
  val R = 6372.8  //radius in km

  def haversine(location1: Location, location2: Location)={
    val dLat=(location2.lat - location1.lat).toRadians
    val dLon=(location2.long - location1.long).toRadians

    val a = pow(sin(dLat/2),2) + pow(sin(dLon/2),2) * cos(location1.lat.toRadians) * cos(location2.lat.toRadians)
    val c = 2 * asin(sqrt(a))
    R * c
  }
}
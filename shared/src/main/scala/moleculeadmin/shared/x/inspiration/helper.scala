package moleculeadmin.shared.x.inspiration

object helper {

  def prettyMillisDelta(millisDelta: Long) = {
    val second = 1000L
    val minute = second * 60
    val hour = minute * 60
    val day = hour * 24
    val month = day * 30
    val year = day * 365

    if(millisDelta / year > 1) millisDelta / year + "years ago"
    else if(millisDelta / year == 1) "1 year"
    else if(millisDelta/ month > 1) millisDelta / month + "months ago"
    else "xxx"
  }

}

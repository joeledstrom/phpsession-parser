import com.joeledstrom.PhpParser

val data =
  """Config|a:1:{s:4:"time";i:1446564104;}Auth|a:1:{s:4:"User";a:5:{s:2:"id";i:1;s:8:"username";s:13:"joel@cake.com";s:4:"role";s:5:"admin";s:7:"created";O:14:"Cake\I18n\Time":3:{s:4:"date";s:26:"2015-11-03 01:34:19.000000";s:13:"timezone_type";i:3;s:8:"timezone";s:3:"UTC";}s:8:"modified";O:14:"Cake\I18n\Time":3:{s:4:"date";s:26:"2015-11-03 01:34:19.000000";s:13:"timezone_type";i:3;s:8:"timezone";s:3:"UTC";}}}Flash|a:0:{}"""

val s = """a:1:{s:4:"User";a:5:{s:2:"id";i:1;s:8:"username";s:13:"joel@cake.com";s:4:"role";s:5:"admin";s:7:"created";O:14:"Cake\I18n\Time":3:{s:4:"date";s:26:"2015-11-03 01:34:19.000000";s:13:"timezone_type";i:3;s:8:"timezone";s:3:"UTC";}s:8:"modified";O:14:"Cake\I18n\Time":3:{s:4:"date";s:26:"2015-11-03 01:34:19.000000";s:13:"timezone_type";i:3;s:8:"timezone";s:3:"UTC";}}}"""


println(PhpParser.parseSerialized(s))
println(PhpParser.parseSession(data))
println(PhpParser.parseSerialized("i:1004534534589898;"))

@echo off

SET VM=java
SET OPTIONS=-Xms128M -Xmx10g -splash:

@echo on
%VM% %OPTIONS% -jar jams-starter.jar -n -m data\j2k_gehlberg\j2k_gehlberg.jam

pause
@echo off

SET VM=java
SET OPTIONS=-Xms128M -Xmx10g

@echo on
%VM% %OPTIONS% -jar juice-starter.jar %*
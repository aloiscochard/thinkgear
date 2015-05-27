NeuroSky ThinkGear Connector
===================
Haskell implementation of the ThinkGear Protocol.

This library contains the codecs and the basic infrastructure to interface with a ThinkGear compatible device (NeuroSky MindWave Mobile/MindFlex) using a bluetooth connection.  

Connecting Devices
-------------

#### NeuroSky BrainWave Mobile
##### Linux
- Connecting the device via `bluetoothctl`:
 
		power on 
		scan on 
		agent on 
		default-agent 
		exit

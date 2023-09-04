
# Documentation

## Sensors & Zones

A Zone consists of two different sensors (in the case of the train stations the zone only have one sensor). When a sensor is activated, the zone toggles it's "active" status.

This gives us the following functionality:
- When a train passes a sensor, the zone for that sensor is now active.
- When the same train passes the second sensor, the zone now becomes inactive.

This way we know if a track-segment is occupied or not.

*The following picture displays our sensor placement:*

![zones](attachments/zones.png)


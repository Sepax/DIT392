package TSim;

import java.util.ArrayList;

import java.awt.Point;

public class TZone {
    private String name;
    private Point[] sensorPair;
    private boolean active;

    public TZone(String name, Point s1, Point s2) {
        this.name = name;
        this.sensorPair = new Point[2];
        this.sensorPair[0] = s1;
        this.sensorPair[1] = s2;
        this.active = false;
    }

    public void refresh(SensorEvent event) {
        if (event.getXpos() != sensorPair[0].getX() || event.getYpos() != sensorPair[0].getY()) {
            return;
        }
        if (event.getStatus() == SensorEvent.ACTIVE) {
            toggle();
        }
    }
    // toggleZone
    // If a sensor becomes active:
    // toggle "active" boolean.

    private void toggle() {
        active = !active;
        System.out.println(name + " is now " + (active ? "active" : "inactive"));
    }

}

package TSim;

import java.util.ArrayList;
import java.util.Collections;
import java.util.concurrent.Semaphore;

public class TZone {
    private String name;
    private ArrayList<TSensor> sensors;
    private Semaphore sem;
    private TSimInterface tsi;

    public TZone(TSimInterface tsi, String name, boolean startZone) {
        this.name = name;
        this.sem = new Semaphore(1);
        this.tsi = tsi;
        this.sensors = new ArrayList<>();

        try {
            if (startZone) {
                sem.acquire();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public TZone(TSimInterface tsi, String name) {
        this(tsi, name, false);
    }

    public String getName() {
        return name;
    }

    public void addSensor(TSensor sensor) {
        sensors.add(sensor);
    }

    public ArrayList<TSensor> getSensors() {
        return sensors;
    }

    public Semaphore getSemaphore() {
        return sem;
    }

    public boolean isActive() {
        return sem.availablePermits() == 0;
    }

    // Check if the sensor event is one of the sensors in the zone.
    public boolean hasSensor(SensorEvent event) {
        if (sensors.isEmpty()) {
            return false;
        }
        for (TSensor sensor : sensors) {
            if (sensor.getX() == event.getXpos() && sensor.getY() == event.getYpos()) {
                return true;
            }
        }
        return false;
    }

    public TSensor getSensor(SensorEvent event) {
        if (sensors.isEmpty()) {
            return null;
        }
        for (TSensor sensor : sensors) {
            if (sensor.getX() == event.getXpos() && sensor.getY() == event.getYpos()) {
                return sensor;
            }
        }
        return null;
    }

    public void printStatus() {
        System.out.print("     \u2514");
        for (int i = 0; i < 32; i++) {
            System.out.print("\u2500");
        }
        System.out.println("> " + name + " : " + (isActive() ? "ACTIVE" : "INACTIVE"));
    }
}

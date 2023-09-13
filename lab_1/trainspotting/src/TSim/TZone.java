package TSim;

import TSim.*;

import java.util.concurrent.Semaphore;

public class TZone {
    private String name;
    private TSensor[] sensorPair;
    private Semaphore sem;
    private TSimInterface tsi;
    private boolean active;

    public TZone(TSimInterface tsi, String name, TSensor s1, TSensor s2, boolean startZone) {
        this.name = name;
        this.sensorPair = new TSensor[2];
        this.sensorPair[0] = s1;
        this.sensorPair[1] = s2;
        this.sem = new Semaphore(1);
        this.tsi = tsi;
        try {
            if (startZone) {
                sem.acquire();
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public TZone(TSimInterface tsi, String name, TSensor s1, TSensor s2) {
        this(tsi, name, s1, s2, false);
    }

    public Semaphore getSemaphore() {
        return sem;
    }

    public boolean isActive() {
        // return sem.availablePermits() == 0;
        return active;
    }

    public TSensor[] getSensors() {
        return sensorPair;
    }

    public void refresh(SensorEvent event, int speed) throws InterruptedException, CommandException {
        for (TSensor sensor : sensorPair) {
            if (!sensorMatch(event, sensor)) {
                continue;
            }
            if (event.getStatus() == SensorEvent.ACTIVE) {
                toggle(event, sensor, speed);
                printStatus();
            }
        }
    }

    public void adjustSwitch(TSensor sensor) {
        return;
    }

    private void toggle(SensorEvent event, TSensor sensor, int speed) throws InterruptedException, CommandException {
        TZone nextZone;
        TZone currentZone = this;

        if (sensor.isStation() && event.getStatus() == SensorEvent.ACTIVE) {
            tsi.setSpeed(event.getTrainId(), 0);
            Thread.sleep((1000 + (20 * Math.abs(speed))));
            speed = -speed;
            tsi.setSpeed(event.getTrainId(), speed);
            return;
        }

        if (!sensor.hasFreeZone()) {
            tsi.setSpeed(event.trainId, 0);
            nextZone = sensor.getAdjZones().get(0);
            Thread.sleep((1000 + (20 * Math.abs(speed))));
            nextZone.getSemaphore().acquire();
            tsi.setSpeed(event.trainId, speed);
            nextZone.getSemaphore().release();
        } else {
            nextZone = sensor.getFreeZone();
        }

        sensor.adjustSwitch(currentZone, nextZone);

        if (isActive() && !sensor.isStation()) {
            sem.release();
        } else {
            sem.acquire();
        }
    }

    // Check if the sensor event is one of the sensors in the zone.
    private boolean sensorMatch(SensorEvent event, TSensor sensor) {
        return event.getXpos() == sensor.getX() && event.getYpos() == sensor.getY();
    }

    private void printStatus() {
        System.out.print("     \u2514");
        for (int i = 0; i < 32; i++) {
            System.out.print("\u2500");
        }
        System.out.println("> " + name + " : " + (isActive() ? "ACTIVE" : "INACTIVE"));
    }
}

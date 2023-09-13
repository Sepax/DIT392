package TSim;

import java.util.ArrayList;

public class TSensor {
    private TSimInterface tsi;

    private int xPos;
    private int yPos;

    private ArrayList<TZone> adjZones;
    private TSwitch tSwitch;

    private boolean hasSwitch;
    private boolean station;

    public TSensor(TSimInterface tsi, int xPos, int yPos, boolean station) {
        this.xPos = xPos;
        this.yPos = yPos;
        this.station = station;
        this.adjZones = new ArrayList<>();
        this.hasSwitch = false;
        this.tsi = TSimInterface.getInstance();
    }

    public TSensor(TSimInterface tsi, int xPos, int yPos) {
        this(tsi, xPos, yPos, false);
    }

    public void addAdjZone(TZone zone) {
        adjZones.add(zone);
    }

    public void addSwitch(TSwitch tswitch) {
        this.tSwitch = tswitch;
        this.hasSwitch = true;
    }

    public int getX() {
        return xPos;
    }

    public int getY() {
        return yPos;
    }

    public void adjustSwitch(TZone currentZone, TZone nextZone) throws CommandException {
        if (!currentZone.isActive() || !hasSwitch) {
            return;
        }

        int dir;

        if (tSwitch.getRightZones().contains(currentZone) && tSwitch.getRightZones().contains(nextZone)) {
            dir = 1;
        } else {
            dir = 0;
        }

        tsi.setSwitch(tSwitch.getX(), tSwitch.getY(), dir); // PROBLEM SKER
    }

    public ArrayList<TZone> getAdjZones() {
        return adjZones;
    }

    public boolean hasFreeZone() {
        if (adjZones.isEmpty()) {
            return false;
        }
        for (TZone zone : adjZones) {
            if (!zone.isActive()) {
                return true;
            }
        }
        return false;
    }

    public TZone getFreeZone() {
        for (TZone zone : adjZones) {
            if (!zone.isActive()) {
                return zone;
            }
        }
        return null;
    }

    public boolean isStation() {
        return station;
    }
}

package TSim;

import java.util.ArrayList;

public class TSensor {
    private TSimInterface tsi;

    private int xPos;
    private int yPos;

    private ArrayList<TZone> adjZones;
    private TSwitch tSwitch;

    private boolean hasSwitch;
    private sType type;

    public enum sType {
        NORMAL, STATION, ACQUIRE, RELEASE
    }

    public TSensor(TSimInterface tsi, int xPos, int yPos, sType type) {
        this.xPos = xPos;
        this.yPos = yPos;
        this.adjZones = new ArrayList<>();
        this.hasSwitch = false;
        this.type = type;
        this.tsi = TSimInterface.getInstance();
    }

    public TSensor(TSimInterface tsi, int xPos, int yPos) {
        this(tsi, xPos, yPos, sType.NORMAL);
    }

    public sType getType() {
        return type;
    }

    public void addAdjZone(TZone zone) {
        adjZones.add(zone);
    }

    public void addSwitch(TSwitch tswitch) {
        this.tSwitch = tswitch;
        this.hasSwitch = true;
    }

    public TSwitch getSwitch() {
        return tSwitch;
    }

    public boolean hasSwitch() {
        return hasSwitch;
    }

    public int getX() {
        return xPos;
    }

    public int getY() {
        return yPos;
    }

    public void adjustSwitch(TZone currentZone, TZone nextZone) throws CommandException {
        if (!hasSwitch) {
            return;
        }

        tSwitch.adjust(currentZone, nextZone);
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
        return type == sType.STATION;
    }
}

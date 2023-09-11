package TSim;

import java.util.ArrayList;

public class TSensor {
    private int xPos;
    private int yPos;

    private ArrayList<TZone> adjZones;
    // Here should be a switch

    public TSensor(int xPos, int yPos) {
        this.xPos = xPos;
        this.yPos = yPos;
        this.adjZones = new ArrayList<>();
    }

    public void addAdjZone(TZone zone) {
        adjZones.add(zone);
    }

    public int getX() {
        return xPos;
    }

    public int getY() {
        return yPos;
    }

    public ArrayList<TZone> getAdjZones() {
        return adjZones;
    }

    public boolean hasFreeZone() {
        if (adjZones.isEmpty()) {
            return false;
        }
        for (TZone zone : adjZones) {
            if (zone.isActive()) {
                return true;
            }
        }
        return false;
    }

    public TZone getFreeZone() {
        for (TZone zone : adjZones) {
            if (zone.isActive()) {
                return zone;
            }
        }
        return new TZone("ZONE NULL", new TSensor(0, 0), new TSensor(0, 0), false);
    }
}

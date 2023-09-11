package TSim;

import java.util.ArrayList;

public class TSensor {
    private int xPos;
    private int yPos;

    private TSimInterface tsim;
    private ArrayList<TZone> adjZones;
    public TSwitch tSwitch;
    boolean hasSwitch;

    public TSensor(int xPos, int yPos) {
        this.xPos = xPos;
        this.yPos = yPos;
        this.adjZones = new ArrayList<>();
        this.hasSwitch = false;
        this.tsim = TSimInterface.getInstance();
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

        tsim.setSwitch(tSwitch.getX(), tSwitch.getY(), dir); // PROBLEM SKER
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
}

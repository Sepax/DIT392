package TSim;

import java.util.ArrayList;

public class TSwitch {
    private int xPos;
    private int yPos;
    private ArrayList<TZone> rightZones;
    private ArrayList<TZone> leftZones;
    private TSimInterface tsi;

    public enum Dir {
        LEFT, RIGHT
    }

    public TSwitch(TSimInterface tsi, int xPos, int yPos) {
        this.xPos = xPos;
        this.yPos = yPos;
        this.rightZones = new ArrayList<>();
        this.leftZones = new ArrayList<>();
        this.tsi = tsi;
    }

    public void adjust(TZone currentZone, TZone nextZone) throws CommandException {
        if (nextZone.isActive()) {
            return;
        }
        int dir;

        if (rightZones.contains(currentZone) && rightZones.contains(nextZone)) {
            dir = 1;
        } else {
            dir = 0;
        }

        tsi.setSwitch(xPos, yPos, dir);
    }

    public void addZone(TZone zone, Dir dir) {
        if (dir == Dir.LEFT) {
            leftZones.add(zone);
        } else {
            rightZones.add(zone);
        }
    }

    public ArrayList<TZone> getZones(Dir dir) {
        if (dir == Dir.LEFT) {
            return leftZones;
        } else {
            return rightZones;
        }
    }

    public int getX() {
        return xPos;
    }

    public int getY() {
        return yPos;
    }

}

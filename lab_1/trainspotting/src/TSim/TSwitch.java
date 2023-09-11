package TSim;

import java.util.ArrayList;

public class TSwitch {
    private int xPos;
    private int yPos;
    private ArrayList<TZone> rightZones;
    private ArrayList<TZone> leftZones;

    public TSwitch(int xPos, int yPos) {
        this.xPos = xPos;
        this.yPos = yPos;
        this.rightZones = new ArrayList<>();
        this.leftZones = new ArrayList<>();
    }

    public void addRightZone(TZone zone) {
        rightZones.add(zone);
    }

    public void addLeftZone(TZone zone) {
        leftZones.add(zone);
    }

    public ArrayList<TZone> getRightZones() {
        return rightZones;
    }

    public ArrayList<TZone> getLeftZones() {
        return leftZones;
    }

    public int getX() {
        return xPos;
    }

    public int getY() {
        return yPos;
    }

}

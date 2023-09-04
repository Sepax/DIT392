package TSim;


public class TZone {
    public String   TZoneName;
    public SensorEvent[] sensorPair;
    public boolean active;
    
    public TZone( String name , SensorEvent x , SensorEvent y ){
        this.TZoneName  = name;
        this.sensorPair = new SensorEvent[2];
        this.sensorPair[0] = x;
        this.sensorPair[1] = y;
        this.active = false;
    } 

    // toggleZone
    //      If a sensor becomes active:
    //      toggle "active" boolean.

     void toggle (){
        if (active == false) { active = true;  }
        else                 { active = false; }
        System.out.println("toggled!");
    }
    
}


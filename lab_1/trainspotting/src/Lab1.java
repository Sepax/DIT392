import TSim.*;
import java.awt.Point;
import java.util.ArrayList;

public class Lab1 {
	private ArrayList<TZone> zones = new ArrayList<>();

	public Lab1(int speed1, int speed2) {
		TSimInterface tsi = TSimInterface.getInstance();
		this.zones = zoneConfig(tsi);

		try {
			tsi.setDebug(true);
			tsi.setSpeed(1, speed1);
			tsi.setSpeed(2, speed2);
			Thread th1 = new Thread(new TrainHandler(1, speed1, zones, tsi));
			Thread th2 = new Thread(new TrainHandler(2, speed2, zones, tsi));
			th1.start();
			th2.start();
		} catch (CommandException e) {
			e.printStackTrace(); // or only e.getMessage() for the error
			System.exit(1);
		}
	}

	public class TrainHandler implements Runnable {
		TSimInterface tsi;
		private int id;
		private int speed;
		private ArrayList<TZone> zones;

		public TrainHandler(int id, int speed, ArrayList<TZone> zones, TSimInterface tsi) {
			this.tsi = tsi;
			this.id = id;
			this.speed = speed;
			this.zones = zones;
		}

		@Override
		public void run() {
			while (true) {
				try {
					SensorEvent sEvent = tsi.getSensor(id);
					for (TZone zone : zones) {
						zone.refresh(sEvent);
					}
				} catch (Exception e) {
					e.printStackTrace();
				}
			}
		}
	}

	private ArrayList<TZone> zoneConfig(TSimInterface tsi) {
		// create sensors
		TSensor A1 = new TSensor(15, 3, true);
		TSensor B1 = new TSensor(7, 7, false);
		TSensor A2 = new TSensor(15, 5, true);
		TSensor B2 = new TSensor(16, 8, false);
		TSensor A3 = new TSensor(9, 7, false);
		TSensor B3 = new TSensor(15, 7, false);
		TSensor A4 = new TSensor(18, 7, false);
		TSensor B4 = new TSensor(16, 9, false);
		TSensor A5 = new TSensor(14, 10, false);
		TSensor B5 = new TSensor(5, 10, false);
		TSensor A6 = new TSensor(14, 9, false);
		TSensor B6 = new TSensor(5, 9, false);
		TSensor A7 = new TSensor(3, 9, false);
		TSensor B7 = new TSensor(2, 11, false);
		TSensor A8 = new TSensor(6, 11, true);
		TSensor B8 = new TSensor(15, 11, false);
		TSensor A9 = new TSensor(4, 13, false);
		TSensor B9 = new TSensor(15, 13, true);

		// create zones (stations are start zones)
		TZone Z1 = new TZone("ZONE 1", A1, B1, true, tsi);
		TZone Z2 = new TZone("ZONE 2", A2, B2, false, tsi);
		TZone Z3 = new TZone("ZONE 3", A3, B3, false, tsi);
		TZone Z4 = new TZone("ZONE 4", A4, B4, false, tsi);
		TZone Z5 = new TZone("ZONE 5", A5, B5, false, tsi);
		TZone Z6 = new TZone("ZONE 6", A6, B6, false, tsi);
		TZone Z7 = new TZone("ZONE 7", A7, B7, false, tsi);
		TZone Z8 = new TZone("ZONE 8", A8, B8, true, tsi);
		TZone Z9 = new TZone("ZONE 9", A9, B9, false, tsi);

		// add adjacent zones to sensors
		A3.addAdjZone(Z1);
		B1.addAdjZone(Z2);
		A4.addAdjZone(Z2);
		A3.addAdjZone(Z2);
		B1.addAdjZone(Z3);
		A4.addAdjZone(Z3);
		B3.addAdjZone(Z4);
		B2.addAdjZone(Z4);
		A6.addAdjZone(Z4);
		A5.addAdjZone(Z4);
		B4.addAdjZone(Z5);
		A7.addAdjZone(Z5);
		B4.addAdjZone(Z6);
		A7.addAdjZone(Z6);
		B6.addAdjZone(Z7);
		B5.addAdjZone(Z7);
		A8.addAdjZone(Z7);
		A9.addAdjZone(Z7);
		B7.addAdjZone(Z8);
		B7.addAdjZone(Z9);

		// create switches depending on the switch direction
		TSwitch S1 = new TSwitch(17, 7);
		TSwitch S2 = new TSwitch(15, 9);
		TSwitch S3 = new TSwitch(4, 9);
		TSwitch S4 = new TSwitch(3, 11);

		// add zones for each "direction" of the switches
		S1.addLeftZone(Z3);
		S1.addLeftZone(Z4);
		S1.addRightZone(Z2);
		S1.addRightZone(Z4);
		S2.addLeftZone(Z4);
		S2.addLeftZone(Z6);
		S2.addRightZone(Z4);
		S2.addRightZone(Z5);
		S3.addRightZone(Z6);
		S3.addRightZone(Z7);
		S3.addLeftZone(Z5);
		S3.addLeftZone(Z7);
		S4.addRightZone(Z7);
		S4.addRightZone(Z8);
		S4.addLeftZone(Z7);
		S4.addLeftZone(Z9);

		// add switches to sensors that will trigger them
		B3.addSwitch(S1);
		A4.addSwitch(S1);
		B2.addSwitch(S1);
		B4.addSwitch(S2);
		A6.addSwitch(S2);
		A5.addSwitch(S2);
		B6.addSwitch(S3);
		B5.addSwitch(S3);
		A7.addSwitch(S3);
		B7.addSwitch(S4);
		A8.addSwitch(S4);
		A9.addSwitch(S4);

		// add zones to list
		ArrayList<TZone> z = new ArrayList<>();
		z.add(Z1);
		z.add(Z2);
		z.add(Z3);
		z.add(Z4);
		z.add(Z5);
		z.add(Z6);
		z.add(Z7);
		z.add(Z8);
		z.add(Z9);

		return z;
	}
}

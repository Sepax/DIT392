import TSim.*;
import java.awt.Point;
import java.util.ArrayList;

public class Lab1 {
	public Lab1(int speed1, int speed2) {
		TSimInterface tsi = TSimInterface.getInstance();
		ArrayList<TZone> zones;

		try {
			tsi.setDebug(true);

			// add sensors

			TSensor A1 = new TSensor(15, 3);
			TSensor B1 = new TSensor(7, 7);

			TSensor A2 = new TSensor(15, 5);
			TSensor B2 = new TSensor(16, 8);

			TSensor A3 = new TSensor(9, 7);
			TSensor B3 = new TSensor(15, 7);

			TSensor A4 = new TSensor(18, 7);
			TSensor B4 = new TSensor(16, 9);

			TSensor A5 = new TSensor(14, 10);
			TSensor B5 = new TSensor(5, 10);

			TSensor A6 = new TSensor(14, 9);
			TSensor B6 = new TSensor(5, 9);

			TSensor A7 = new TSensor(3, 9);
			TSensor B7 = new TSensor(2, 11);

			TSensor A8 = new TSensor(6, 11);
			TSensor B8 = new TSensor(15, 11);

			TSensor A9 = new TSensor(4, 13);
			TSensor B9 = new TSensor(15, 13);

			// create zones
			TZone Z1 = new TZone("ZONE 1", A1, B1, true);
			TZone Z2 = new TZone("ZONE 1", A2, B2, false);
			TZone Z3 = new TZone("ZONE 1", A3, B3, false);
			TZone Z4 = new TZone("ZONE 1", A4, B4, false);
			TZone Z5 = new TZone("ZONE 1", A5, B5, false);
			TZone Z6 = new TZone("ZONE 1", A6, B6, false);
			TZone Z7 = new TZone("ZONE 1", A7, B7, false);
			TZone Z8 = new TZone("ZONE 1", A8, B8, true);
			TZone Z9 = new TZone("ZONE 1", A9, B9, false);

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

			tsi.setSpeed(1, speed1);
			tsi.setSpeed(2, speed2);
		} catch (CommandException e) {
			e.printStackTrace(); // or only e.getMessage() for the error
			System.exit(1);
		}
	}
}

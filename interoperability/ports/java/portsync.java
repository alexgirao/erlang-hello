
/*
 */

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

class portsync {
    public static void main(String[] Args) throws Exception {
	byte[] buffer = new byte[0xffff];

	for (;;) {
	    int n = System.in.read(buffer, 0, buffer.length);
	    if (n == -1) {
		System.err.println("closed");
		System.exit(0);
		return;
	    }

	    assert n > 0;

	    hexdump.dump(buffer, 0, System.err, 0, n);

	    byte result = 0;

	    if (buffer[4] == 1) {
		result = (byte)(buffer[5] + 1);
	    } else if (buffer[4] == 2) {
		result = (byte)(buffer[5] * 2);
	    } else {
		throw new RuntimeException("exhaustion");
	    }

	    System.out.write(0);
	    System.out.write(0);
	    System.out.write(0);
	    System.out.write(1);
	    System.out.write(result);
	    System.out.flush();      // don't use buffered streams, flush immediately
	}
    }
}

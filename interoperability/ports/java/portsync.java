
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
	    int n = sync_read(System.in, buffer);
	    assert n > 0;

	    hexdump.dump(buffer, 0, System.err, 0, n);

	    System.out.write(0);
	    System.out.write(0);
	    System.out.write(0);
	    System.out.write(1);
	    System.out.write(123);
	    System.out.flush();      // don't use buffered streams, flush immediately
	}
    }

    private static int sync_read(final InputStream src, byte[] buffer) throws java.io.IOException {
	int n = src.read(buffer, 0, buffer.length);
	assert n >= 0;
	return n;
    }
}

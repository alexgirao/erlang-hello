
/*
 */

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

class port {
    public static void main(String[] Args) throws Exception {
	byte[] buffer = new byte[0xffff];

	for (;;) {
	    int n = async_read(System.in, buffer);
	    assert n >= 0;
	    if (n > 0) {
		hexdump.dump(buffer, 0, System.err, 0, n);

		System.out.write(0);
		System.out.write(0);
		System.out.write(0);
		System.out.write(1);
		System.out.write(123);
		System.out.flush();      // don't use buffered streams, flush immediately
	    } else {
		// sleep here, so we don't stress cpu
		Thread.sleep(10); // miliseconds
	    }
	}
    }

    private static int async_read(final InputStream src, byte[] buffer) throws java.io.IOException {
	int len_available = src.available();

	assert len_available >= 0;

	if (len_available > 0) {
	    int len = Math.min(len_available, buffer.length);

	    int n = src.read(buffer, 0, len);
	    assert n == len;

	    return n;
	}

	return len_available;
    }
}

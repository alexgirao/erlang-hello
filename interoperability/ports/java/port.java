
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
	    int n = read(System.in, buffer);
	    hexdump.dump(buffer, 0, System.err, 0, n);
	}
    }

    private static int read(final InputStream src, byte[] buffer) throws java.io.IOException {
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

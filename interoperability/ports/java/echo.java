/* javac -cp .:../../java/OtpErlang-R13B01.jar echo.java
 */

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.IOException;

import com.ericsson.otp.erlang.*;

class echo {
    public static void main(String[] Args) throws Exception {
	for (;;) {
	    /* read packet size
	     */

	    byte[] buffer = new byte[4];
	    int n = System.in.read(buffer);
	    if (n != 4) {
		throw new Exception("failed to read packet length");
	    }
	    
	    int packet_size = read4BE(buffer, 0);
	    if (packet_size < 0) {
		throw new Exception("invalid packet length");
	    }
	    
	    //System.err.printf("packet size: %d\n", packet_size);

	    /* read binary term
	     */

	    byte[] binary = new byte[packet_size];
	    n = System.in.read(binary);

	    if (n != packet_size) {
		throw new Exception("failed to read all packet data");
	    }

	    OtpErlangObject term = binary_to_term(binary);
	
	    /* echo
	     */

	    OtpOutputStream out = new OtpOutputStream();

	    out.write1(OtpExternal.versionTag);
	    out.write_any(term);

	    System.out.write(hton4(out.size()));  // packet size
	    out.writeTo(System.out);
	    System.out.flush();
	}
    }

    public static int read4BE(byte[] b, int offset) {
        return (b[0+offset] << 24 & 0xff000000) + (b[1+offset] << 16 & 0xff0000)
	    + (b[2+offset] << 8 & 0xff00) + (b[3+offset] & 0xff);
    }

    public static byte[] hton4(long v) {
	byte[] buffer = new byte[4];
	buffer[0] = (byte) ((v >> 24) & 0xff);
	buffer[1] = (byte) ((v >> 16) & 0xff);
	buffer[2] = (byte) ((v >> 8) & 0xff);
	buffer[3] = (byte) ((v) & 0xff);
	return buffer;
    }

    public static OtpErlangObject binary_to_term(byte[] buffer) throws Exception {
	OtpInputStream in = new OtpInputStream(buffer);

	int versionTag = in.read1();
	if (versionTag != OtpExternal.versionTag) {
	    throw new OtpErlangDecodeException("versionTag doesn't match");
	}
	    
	OtpErlangObject e_obj = in.read_any();
	in.close();

	return e_obj;
    }

}

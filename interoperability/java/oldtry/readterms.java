/* javac -cp OtpErlang-R13B01.jar readterms.java && java -ea -cp .:OtpErlang-R13B01.jar readterms
 */

import java.io.InputStream;
import java.io.IOException;
import java.io.File;
import java.io.FileInputStream;

import com.ericsson.otp.erlang.*;

class readterms {
    public static void main(String[] Args) throws java.io.FileNotFoundException, java.io.IOException, OtpErlangDecodeException {
	File f0 = new File("sampledata.bin");
	byte[] buffer = getBytesFromFile(f0);
	OtpInputStream in = new OtpInputStream(buffer);

	//OtpErlangObject term = in.read_any();
	//System.out.println(term);

	System.out.printf("total bytes: %d (0x%x)\n", buffer.length, buffer.length);

	for (;;) {
	    int pos_a = in.getPos();
	    OtpErlangObject e_obj = in.read_any();
	    int pos_b = in.getPos();

	    System.out.printf("0x%08x - 0x%08x: %s\n", pos_a, pos_b, e_obj);
	    System.out.flush();

	    if (pos_b == buffer.length) {
		break;
	    }
	}
    }

    public static byte[] getBytesFromFile(File file) throws IOException {
	InputStream is = new FileInputStream(file);
	long length = file.length();


	if (length > Integer.MAX_VALUE) {
	    throw new IOException("file too large");
	}

	byte[] bytes = new byte[(int)length];

	int offset = 0;
	int numRead = 0;
	while (offset < bytes.length
	       && (numRead = is.read(bytes, offset, bytes.length - offset)) >= 0) {
	    offset += numRead;
	}

	if (offset < bytes.length) {
	    throw new IOException("Could not completely read file " + file.getName());
	}

	is.close();
	return bytes;
    }
}

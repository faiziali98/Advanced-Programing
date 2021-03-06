public class Rectangle {
	public float left;
	public float right;
	public float top;
	public float bottom;

	public Rectangle(float l, float r, float t, float b) {
		left   = l;
		right  = r;
		top    = t;
		bottom = b;
	}

	public Rectangle encompass(Rectangle that) {
		return new Rectangle(Math.min(this.left,   that.left),
						     Math.max(this.right,  that.right),
						     Math.max(this.top,    that.top),
				             Math.min(this.bottom, that.bottom));
	}

	public boolean contains(float x, float y, int i){
		return x>=left && x<=right && y>=bottom && y<=top;
	}
	public String toString() {
		return "[left=" + left + " right=" + right + " top=" + top + " bottom=" + bottom + "]";
	}
}

abstract class Karatsuba(level : Int) {
    private val mLevel : Int = level;

    abstract val name : String

    abstract fun compute() : Any
}

class KaratsubaBox(level: Int, val a: Array<Long>, val b: Array<Long>): Karatsuba(level) {

    override val name = "KaratsubaBox"

    override fun compute() : Array<Long> {
        val halfSize : Int = a.size / 2;

        val r : Array<Long> = Array<Long>(a.size + b.size, {0});

        val z0 : Array<Long> = productScan(a.sliceArray(0 until halfSize), b.sliceArray(0 until halfSize))

        val z2 : Array<Long> = productScan(a.sliceArray(halfSize until a.size), b.sliceArray(halfSize until b.size))

        val mid : Pair<Array<Long>, Array<Long>> = Pair(Array<Long>(halfSize, {0}), Array<Long>(halfSize, {0}))
        for (i in mid.first.indices) {
            mid.first[i] = a[i] + a[halfSize + i]
            mid.second[i] = b[i] + b[halfSize + i]
        }

        val z1 : Array<Long> = productScan(mid.first, mid.second)

        for (i in z1.indices) {
            z1[i] = z1[i] - z0[i] - z2[i];
        }

        for (i in a.indices) {
            r[i] += z0[i]
            r[halfSize + i] += z1[i]
            r[a.size + i] += z2[i]
        }

        return r
    }

    fun productScan(scanA : Array<Long>, scanB : Array<Long>) : Array<Long> {
        val r : Array<Long> = Array<Long>(scanA.size + scanB.size, {0});

        for (i in 0 until scanA.size) {
            for (j in 0 until scanB.size) {
                r[i + j] += scanA[i] * scanB[j];
            }
        }

        return r;
    }
}

class KaratsubaNonBox(level: Int, val a: LongArray, val b: LongArray): Karatsuba(level) {

    override val name = "KaratsubaNonBox"

    override fun compute() : LongArray {
        val halfSize : Int = a.size / 2;

        val r : LongArray = LongArray(a.size + b.size, {0});

        val z0 : LongArray = productScan(a.sliceArray(0 until halfSize), b.sliceArray(0 until halfSize))

        val z2 : LongArray = productScan(a.sliceArray(halfSize until a.size), b.sliceArray(halfSize until b.size))

        val mid : Pair<LongArray, LongArray> = Pair(LongArray(halfSize, {0}), LongArray(halfSize, {0}))
        for (i in mid.first.indices) {
            mid.first[i] = a[i] + a[halfSize + i]
            mid.second[i] = b[i] + b[halfSize + i]
        }

        val z1 : LongArray = productScan(mid.first, mid.second)

        for (i in z1.indices) {
            z1[i] = z1[i] - z0[i] - z2[i];
        }

        for (i in a.indices) {
            r[i] += z0[i]
            r[halfSize + i] += z1[i]
            r[a.size + i] += z2[i]
        }

        return r
    }

    fun productScan(scanA : LongArray, scanB : LongArray) : LongArray {
        val r : LongArray = LongArray(scanA.size + scanB.size, {0});

        for (i in 0 until scanA.size) {
            for (j in 0 until scanB.size) {
                r[i + j] += scanA[i] * scanB[j];
            }
        }

        return r;
    }
}
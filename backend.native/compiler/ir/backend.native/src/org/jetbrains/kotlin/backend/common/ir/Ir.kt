package org.jetbrains.kotlin.backend.common.ir

import org.jetbrains.kotlin.backend.common.CommonBackendContext
import org.jetbrains.kotlin.backend.common.descriptors.getFunction
import org.jetbrains.kotlin.backend.common.descriptors.replace
import org.jetbrains.kotlin.backend.konan.KonanBuiltIns
import org.jetbrains.kotlin.builtins.KotlinBuiltIns
import org.jetbrains.kotlin.builtins.PrimitiveType
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.incremental.components.NoLookupLocation
import org.jetbrains.kotlin.ir.declarations.IrFunction
import org.jetbrains.kotlin.ir.declarations.IrModuleFragment
import org.jetbrains.kotlin.ir.symbols.IrFunctionSymbol
import org.jetbrains.kotlin.ir.symbols.IrSimpleFunctionSymbol
import org.jetbrains.kotlin.name.FqName
import org.jetbrains.kotlin.name.Name
import org.jetbrains.kotlin.ir.util.SymbolTable
import org.jetbrains.kotlin.types.KotlinType
import org.jetbrains.kotlin.types.SimpleType
import org.jetbrains.kotlin.util.OperatorNameConventions

// This is what Context collects about IR.
abstract class Ir<out T: CommonBackendContext>(val context: T, val irModule: IrModuleFragment) {

    abstract val symbols: Symbols<T>

    val defaultParameterDeclarationsCache = mutableMapOf<FunctionDescriptor, IrFunction>()

    open fun shouldGenerateHandlerParameterForDefaultBodyFun() = false
}

open class Symbols<out T: CommonBackendContext>(val context: T, private val symbolTable: SymbolTable) {

    private val builtIns
        get() = context.builtIns

    private fun builtInsPackage(vararg packageNameSegments: String) =
            context.builtIns.builtInsModule.getPackage(FqName.fromSegments(listOf(*packageNameSegments))).memberScope

    val refClass = symbolTable.referenceClass(context.getInternalClass("Ref"))

    val areEqualByValue = context.getInternalFunctions("areEqualByValue").map {
        symbolTable.referenceSimpleFunction(it)
    }

    val areEqual = symbolTable.referenceSimpleFunction(context.getInternalFunctions("areEqual").single())

    val ThrowNullPointerException = symbolTable.referenceSimpleFunction(
            context.getInternalFunctions("ThrowNullPointerException").single())

    val ThrowNoWhenBranchMatchedException = symbolTable.referenceSimpleFunction(
            context.getInternalFunctions("ThrowNoWhenBranchMatchedException").single())

    val ThrowTypeCastException = symbolTable.referenceSimpleFunction(
            context.getInternalFunctions("ThrowTypeCastException").single())

    val ThrowUninitializedPropertyAccessException = symbolTable.referenceSimpleFunction(
            context.getInternalFunctions("ThrowUninitializedPropertyAccessException").single()
    )

    val stringBuilder = symbolTable.referenceClass(
            builtInsPackage("kotlin", "text").getContributedClassifier(
                    Name.identifier("StringBuilder"), NoLookupLocation.FROM_BACKEND
            ) as ClassDescriptor
    )

    val iterator = symbolTable.referenceClass(
            builtInsPackage("kotlin", "collections").getContributedClassifier(
                    Name.identifier("Iterator"), NoLookupLocation.FROM_BACKEND
            ) as ClassDescriptor)

    private fun progression(name: String) = symbolTable.referenceClass(
            builtInsPackage("kotlin", "ranges").getContributedClassifier(
                    Name.identifier(name), NoLookupLocation.FROM_BACKEND
            ) as ClassDescriptor
    )

    val charProgression = progression("CharProgression")
    val intProgression  = progression("IntProgression")
    val longProgression = progression("LongProgression")
    val progressionClasses = setOf(charProgression, intProgression, longProgression)
    val progressionClassesTypes = progressionClasses.map { it.descriptor.defaultType }.toSet()

    val checkProgressionStep = context.getInternalFunctions("checkProgressionStep")
            .map { Pair(it.returnType, symbolTable.referenceSimpleFunction(it)) }.toMap()
    val getProgressionBound = context.getInternalFunctions("getProgressionBound")
            .map { Pair(it.returnType, symbolTable.referenceSimpleFunction(it)) }.toMap()

    val defaultConstructorMarker = symbolTable.referenceClass(context.getInternalClass("DefaultConstructorMarker"))

    val any = symbolTable.referenceClass(builtIns.any)
    val unit = symbolTable.referenceClass(builtIns.unit)

    val char = symbolTable.referenceClass(builtIns.char)

    val byte = symbolTable.referenceClass(builtIns.byte)
    val short = symbolTable.referenceClass(builtIns.short)
    val int = symbolTable.referenceClass(builtIns.int)
    val long = symbolTable.referenceClass(builtIns.long)

    val integerClasses = setOf(byte, short, int, long)
    val integerClassesTypes: Set<SimpleType> = mutableSetOf<SimpleType>().apply {
        integerClasses.mapTo(this) { it.descriptor.defaultType }
    }

    val arrayOf = symbolTable.referenceSimpleFunction(
            builtInsPackage("kotlin").getContributedFunctions(
                    Name.identifier("arrayOf"), NoLookupLocation.FROM_BACKEND
            ).single()
    )

    val array = symbolTable.referenceClass(builtIns.array)

    private fun primitiveArrayClass(type: PrimitiveType) =
            symbolTable.referenceClass(builtIns.getPrimitiveArrayClassDescriptor(type))

    val byteArray = primitiveArrayClass(PrimitiveType.BYTE)
    val charArray = primitiveArrayClass(PrimitiveType.CHAR)
    val shortArray = primitiveArrayClass(PrimitiveType.SHORT)
    val intArray = primitiveArrayClass(PrimitiveType.INT)
    val longArray = primitiveArrayClass(PrimitiveType.LONG)
    val floatArray = primitiveArrayClass(PrimitiveType.FLOAT)
    val doubleArray = primitiveArrayClass(PrimitiveType.DOUBLE)
    val booleanArray = primitiveArrayClass(PrimitiveType.BOOLEAN)

    val arrays = PrimitiveType.values().map { primitiveArrayClass(it) } + array

    private fun arrayExtensionFun(type: KotlinType, name: String): IrSimpleFunctionSymbol {
        val descriptor = builtInsPackage("kotlin")
                .getContributedFunctions(Name.identifier(name), NoLookupLocation.FROM_BACKEND)
                .singleOrNull { it.valueParameters.isEmpty()
                        && (it.extensionReceiverParameter?.type?.constructor?.declarationDescriptor as? ClassDescriptor)?.defaultType == type }
                ?: throw Error(type.toString())
        return symbolTable.referenceSimpleFunction(descriptor)
    }


    private val arrayTypes = arrayOf(
            builtIns.getPrimitiveArrayKotlinType(PrimitiveType.BYTE),
            builtIns.getPrimitiveArrayKotlinType(PrimitiveType.CHAR),
            builtIns.getPrimitiveArrayKotlinType(PrimitiveType.SHORT),
            builtIns.getPrimitiveArrayKotlinType(PrimitiveType.INT),
            builtIns.getPrimitiveArrayKotlinType(PrimitiveType.LONG),
            builtIns.getPrimitiveArrayKotlinType(PrimitiveType.FLOAT),
            builtIns.getPrimitiveArrayKotlinType(PrimitiveType.DOUBLE),
            builtIns.getPrimitiveArrayKotlinType(PrimitiveType.BOOLEAN),
            builtIns.array.defaultType
    )
    val arrayContentToString = arrayTypes.associateBy({ it }, { arrayExtensionFun(it, "contentToString") })
    val arrayContentHashCode = arrayTypes.associateBy({ it }, { arrayExtensionFun(it, "contentHashCode") })

    val copyRangeTo = arrays.map { symbol ->
        val packageViewDescriptor = builtIns.builtInsModule.getPackage(KotlinBuiltIns.COLLECTIONS_PACKAGE_FQ_NAME)
        val functionDescriptor = packageViewDescriptor.memberScope
                .getContributedFunctions(Name.identifier("copyRangeTo"), NoLookupLocation.FROM_BACKEND)
                .first {
                    it.extensionReceiverParameter?.type?.constructor?.declarationDescriptor == symbol.descriptor
                }
        symbol.descriptor to symbolTable.referenceSimpleFunction(functionDescriptor)
    }.toMap()

    val intAnd = symbolTable.referenceFunction(
            builtIns.intType.memberScope
                    .getContributedFunctions(OperatorNameConventions.AND, NoLookupLocation.FROM_BACKEND)
                    .single()
    )

    val intPlusInt = symbolTable.referenceFunction(
            builtIns.intType.memberScope
                    .getContributedFunctions(OperatorNameConventions.PLUS, NoLookupLocation.FROM_BACKEND)
                    .single {
                        it.valueParameters.single().type == builtIns.intType
                    }
    )



    val valuesForEnum = symbolTable.referenceSimpleFunction(
            context.getInternalFunctions("valuesForEnum").single())

    val valueOfForEnum = symbolTable.referenceSimpleFunction(
            context.getInternalFunctions("valueOfForEnum").single())

    val getContinuation = symbolTable.referenceSimpleFunction(
            context.getInternalFunctions("getContinuation").single())

    val coroutineImpl = symbolTable.referenceClass(context.getInternalClass("CoroutineImpl"))

    val coroutineSuspendedGetter = symbolTable.referenceSimpleFunction(
            builtInsPackage("kotlin", "coroutines", "experimental", "intrinsics")
                    .getContributedVariables(Name.identifier("COROUTINE_SUSPENDED"), NoLookupLocation.FROM_BACKEND)
                    .single().getter!!
    )

    val kFunctionImpl = symbolTable.referenceClass(context.reflectionTypes.kFunctionImpl)

    val kProperty0Impl = symbolTable.referenceClass(context.reflectionTypes.kProperty0Impl)
    val kProperty1Impl = symbolTable.referenceClass(context.reflectionTypes.kProperty1Impl)
    val kProperty2Impl = symbolTable.referenceClass(context.reflectionTypes.kProperty2Impl)
    val kMutableProperty0Impl = symbolTable.referenceClass(context.reflectionTypes.kMutableProperty0Impl)
    val kMutableProperty1Impl = symbolTable.referenceClass(context.reflectionTypes.kMutableProperty1Impl)
    val kMutableProperty2Impl = symbolTable.referenceClass(context.reflectionTypes.kMutableProperty2Impl)
    val kLocalDelegatedPropertyImpl = symbolTable.referenceClass(context.reflectionTypes.kLocalDelegatedPropertyImpl)
    val kLocalDelegatedMutablePropertyImpl = symbolTable.referenceClass(context.reflectionTypes.kLocalDelegatedMutablePropertyImpl)

    fun getFunction(name: Name, receiverType: KotlinType, vararg argTypes: KotlinType) =
        symbolTable.referenceFunction(receiverType.memberScope.getContributedFunctions(name, NoLookupLocation.FROM_BACKEND)
                .single {
                    var i = 0
                    it.valueParameters.size == argTypes.size && it.valueParameters.all { type -> type == argTypes[i++] }
                }
        )

    fun getBinaryOperator(name: Name, lhsType: KotlinType, rhsType: KotlinType) =
        symbolTable.referenceFunction(lhsType.memberScope.getContributedFunctions(name, NoLookupLocation.FROM_BACKEND)
                .single { it.valueParameters.size == 1 && it.valueParameters[0].type == rhsType }
        )

    fun getUnaryOperator(name: Name, receiverType: KotlinType) =
        symbolTable.referenceFunction(receiverType.memberScope.getContributedFunctions(name, NoLookupLocation.FROM_BACKEND)
                .single { it.valueParameters.isEmpty() }
        )
}
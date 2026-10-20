package replique.lang;

import java.lang.reflect.Constructor;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.List;

/**
 * Interop the compiler couldn't resolve: a call to a member of an object whose
 * class isn't known at compile time. Dev mode only, behind a warning;
 * reflection is an error in advanced mode (doc/language.md §6).
 *
 * <p>A member is chosen among the public ones of the right name and number of
 * parameters, the first whose parameters accept the arguments, with numbers
 * converted to a primitive parameter's type.
 */
public final class Reflector {
    private Reflector() {}

    public static Object invokeInstance(Object target, String name, Object[] args) {
        if (target == null) throw new NullPointerException("Cannot call " + name + " on nil");
        List<Method> ms = new ArrayList<>();
        for (Method m : target.getClass().getMethods()) {
            if (m.getName().equals(name) && !Modifier.isStatic(m.getModifiers())
                    && m.getParameterCount() == args.length) ms.add(m);
        }
        return invoke(choose(ms, args, target.getClass(), name), target, args);
    }

    public static Object invokeStatic(Class<?> c, String name, Object[] args) {
        List<Method> ms = new ArrayList<>();
        for (Method m : c.getMethods()) {
            if (m.getName().equals(name) && Modifier.isStatic(m.getModifiers())
                    && m.getParameterCount() == args.length) ms.add(m);
        }
        return invoke(choose(ms, args, c, name), null, args);
    }

    public static Object construct(Class<?> c, Object[] args) {
        List<Constructor<?>> cs = new ArrayList<>();
        for (Constructor<?> k : c.getConstructors()) {
            if (k.getParameterCount() == args.length) cs.add(k);
        }
        Constructor<?> k = choose(cs, args, c, "new");
        try {
            return k.newInstance(convert(k.getParameterTypes(), args));
        } catch (InvocationTargetException e) {
            throw sneaky(e.getCause());
        } catch (ReflectiveOperationException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public static Object field(Object target, String name) {
        if (target == null) throw new NullPointerException("Cannot read the field " + name + " of nil");
        try {
            Field f = target.getClass().getField(name);
            return f.get(target);
        } catch (NoSuchFieldException e) {
            throw new IllegalArgumentException("No field " + name + " in " + target.getClass().getName());
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException(e);
        }
    }

    private static <T extends Executable> T choose(List<T> candidates, Object[] args, Class<?> c, String name) {
        for (T m : candidates) {
            if (accepts(m.getParameterTypes(), args)) return m;
        }
        throw new IllegalArgumentException("No matching method " + name + " taking " + args.length
                + " argument(s) for " + c.getName());
    }

    private static boolean accepts(Class<?>[] params, Object[] args) {
        for (int i = 0; i < params.length; i++) {
            Class<?> p = params[i];
            Object a = args[i];
            if (p.isPrimitive()) {
                if (a == null) return false;
                if (p == boolean.class ? !(a instanceof Boolean)
                        : p == char.class ? !(a instanceof Character)
                        : !(a instanceof Number)) return false;
            } else if (a != null && !p.isInstance(a)) {
                return false;
            }
        }
        return true;
    }

    private static Object[] convert(Class<?>[] params, Object[] args) {
        Object[] out = args.clone();
        for (int i = 0; i < params.length; i++) {
            Class<?> p = params[i];
            if (p.isPrimitive() && out[i] instanceof Number n) {
                if (p == int.class) out[i] = n.intValue();
                else if (p == long.class) out[i] = n.longValue();
                else if (p == double.class) out[i] = n.doubleValue();
                else if (p == float.class) out[i] = n.floatValue();
                else if (p == short.class) out[i] = n.shortValue();
                else if (p == byte.class) out[i] = n.byteValue();
            }
        }
        return out;
    }

    private static Object invoke(Method m, Object target, Object[] args) {
        try {
            return m.invoke(target, convert(m.getParameterTypes(), args));
        } catch (InvocationTargetException e) {
            throw sneaky(e.getCause());
        } catch (IllegalAccessException e) {
            throw new IllegalArgumentException(e);
        }
    }

    @SuppressWarnings("unchecked")
    private static <T extends Throwable> RuntimeException sneaky(Throwable t) throws T {
        throw (T) t;
    }
}

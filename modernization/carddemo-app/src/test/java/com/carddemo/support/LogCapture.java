package com.carddemo.support;

import ch.qos.logback.classic.Level;
import ch.qos.logback.classic.Logger;
import ch.qos.logback.classic.LoggerContext;
import ch.qos.logback.classic.spi.ILoggingEvent;
import ch.qos.logback.classic.spi.IThrowableProxy;
import ch.qos.logback.classic.spi.StackTraceElementProxy;
import ch.qos.logback.core.read.ListAppender;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.slf4j.LoggerFactory;

/**
 * Logback appender capture for log-content assertions (s6.4 PAN check): every event of every logger, with the
 * formatted message and the whole throwable chain, while the listed loggers run at the given level.
 */
public final class LogCapture implements AutoCloseable {

    private final ListAppender<ILoggingEvent> appender = new ListAppender<>();
    private final Logger root;
    private final Map<Logger, Level> previous = new HashMap<>();

    private LogCapture(Level level, String... loggers) {
        LoggerContext context = (LoggerContext) LoggerFactory.getILoggerFactory();
        root = context.getLogger(org.slf4j.Logger.ROOT_LOGGER_NAME);
        for (String name : loggers) {
            Logger logger = context.getLogger(name);
            previous.put(logger, logger.getLevel());
            logger.setLevel(level);
        }
        appender.setContext(context);
        appender.start();
        root.addAppender(appender);
    }

    /** Captures everything; {@code loggers} are lowered to {@code level} until {@link #close()}. */
    public static LogCapture start(Level level, String... loggers) {
        return new LogCapture(level, loggers);
    }

    public int size() {
        return appender.list.size();
    }

    /** One string per event: logger, level, message and the messages and frames of the throwable chain. */
    public List<String> lines() {
        List<String> lines = new ArrayList<>();
        for (ILoggingEvent e : List.copyOf(appender.list)) {
            StringBuilder b = new StringBuilder(e.getLoggerName()).append(' ').append(e.getLevel()).append(' ')
                    .append(e.getFormattedMessage());
            for (IThrowableProxy t = e.getThrowableProxy(); t != null; t = t.getCause()) {
                b.append(" | ").append(t.getClassName()).append(": ").append(t.getMessage());
                for (StackTraceElementProxy frame : t.getStackTraceElementProxyArray()) {
                    b.append(' ').append(frame.getSTEAsString());
                }
            }
            lines.add(b.toString());
        }
        return lines;
    }

    @Override
    public void close() {
        root.detachAppender(appender);
        appender.stop();
        previous.forEach(Logger::setLevel);
    }
}
